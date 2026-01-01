// =============================================================================
// Semantic Synchrony Web UI - Main Application
// =============================================================================

// =============================================================================
// State Management
// =============================================================================

const State = {
    // Connection
    ws: null,
    connected: false,
    serverUrl: 'ws://localhost:8182/gremlin',

    // Configuration from server
    config: null,
    sourcesByName: {},

    // View state
    rootId: null,
    view: null,
    selectedId: null,
    expandedNodes: new Set(),
    history: [],
    forwardHistory: [],
    viewStyle: 'forward',  // 'forward' or 'backward'

    // Filter settings
    filter: {
        excludedSources: new Set(),  // Empty means include all sources
        defaultSource: 'private',
        minWeight: 0.0,
        defaultWeight: 0.5
    },

    // UI state
    searchQuery: '',
    editingNodeId: null,
    pendingCallbacks: {},  // Map of requestId -> callback for concurrent requests
    nextRequestId: 1,
    newNoteMode: null,

    // Emacs-style key chord state
    pendingChord: null,  // e.g., 'C-c' when waiting for next key
    pendingNumericAction: null,  // e.g., 'weight' when waiting for 0-4 digit

    // Clipboard for cut/copy/paste
    clipboard: null,  // { id, title, isCut: boolean }

    // Undo stack - stores operations that can be undone
    // Each entry: { type: 'setProperty'|'delete'|'create'|'move', ...data }
    undoStack: [],
    maxUndoSize: 50,

    // View options
    wrapTitles: true,  // true = wrap long titles, false = single line with scroll
    showLineNumbers: true,  // Show line numbers in left margin

    // Split view
    splitView: false,
    activePane: 0,  // 0 or 1
    panes: [
        { rootId: null, view: null, selectedId: null, expandedNodes: new Set(), history: [], forwardHistory: [], viewStyle: 'forward', viewDepth: 2, lastSearchQuery: null },
        { rootId: null, view: null, selectedId: null, expandedNodes: new Set(), history: [], forwardHistory: [], viewStyle: 'forward', viewDepth: 2, lastSearchQuery: null }
    ]
};

// Special marker for search results that we want to be able to navigate back to
const SEARCH_RESULT_PREFIX = '__search__:';

// Get current pane state
function getPane() {
    return State.panes[State.activePane];
}

// Create a history entry with all information needed to restore a view
function createHistoryEntry(pane) {
    if (!pane.rootId) return null;

    const isSearch = pane.rootId.startsWith(SEARCH_RESULT_PREFIX);
    const entry = {
        id: pane.rootId,
        title: pane.view ? pane.view.title : 'Unknown',
        source: pane.view ? pane.view.source : null,
        viewStyle: pane.viewStyle,
        viewDepth: pane.viewDepth,
        timestamp: Date.now()
    };

    if (isSearch) {
        entry.searchQuery = pane.rootId.substring(SEARCH_RESULT_PREFIX.length);
        entry.isSearch = true;
    }

    return entry;
}

// Check if a source passes the current filter (not in excluded set)
function sourcePassesFilter(source) {
    if (!source) return true;  // Unknown sources pass
    return !State.filter.excludedSources.has(source);
}

// Get the list of included sources for server requests
// Returns empty array if all sources are included (no exclusions)
function getIncludedSourcesForServer() {
    if (State.filter.excludedSources.size === 0) {
        return [];  // Empty means include all
    }
    // Return all sources except excluded ones
    const allSources = State.config.sources.map(s => s.name);
    return allSources.filter(s => !State.filter.excludedSources.has(s));
}

// Build filter object for server requests
function getFilterForServer() {
    return {
        includedSources: getIncludedSourcesForServer(),
        defaultSource: State.filter.defaultSource,
        minWeight: State.filter.minWeight,
        defaultWeight: State.filter.defaultWeight
    };
}

// Navigate to a history entry
function navigateToHistoryEntry(entry, paneIndex) {
    if (entry.isSearch) {
        reExecuteSearch(entry.searchQuery, paneIndex);
    } else {
        if (paneIndex === 0) {
            getView(entry.id, entry.viewDepth);
        } else {
            getViewForPane(entry.id, paneIndex, entry.viewDepth);
        }
    }
}

// Initialize pane 0 to share references with State
function initializePaneState() {
    const pane = State.panes[0];
    pane.history = State.history;
    pane.forwardHistory = State.forwardHistory;
    pane.expandedNodes = State.expandedNodes;
}
initializePaneState();

// =============================================================================
// Color Utilities (matching smsn-mode)
// =============================================================================

function parseColor(numericColor) {
    // Convert numeric color (e.g., 0xff0000) to RGB
    const blue = numericColor % 256;
    const green = Math.floor(numericColor / 256) % 256;
    const red = Math.floor(numericColor / 65536);
    return { r: red, g: green, b: blue };
}

function colorToHex(color) {
    const r = Math.round(color.r).toString(16).padStart(2, '0');
    const g = Math.round(color.g).toString(16).padStart(2, '0');
    const b = Math.round(color.b).toString(16).padStart(2, '0');
    return `#${r}${g}${b}`;
}

function darkenColor(color, factor) {
    // Darken by multiplying RGB values
    return {
        r: color.r * factor,
        g: color.g * factor,
        b: color.b * factor
    };
}

function fadeColor(color, weight) {
    // Fade toward white based on weight (lower weight = more faded)
    // This matches smsn-mode's fade function
    function fade(c, w) {
        const low = c + (255 - c) * 0.9375; // weighted toward white
        const high = c;
        return low + (high - low) * w;
    }
    return {
        r: fade(color.r, weight),
        g: fade(color.g, weight),
        b: fade(color.b, weight)
    };
}

function getNoteColor(source, weight) {
    const sourceConfig = State.sourcesByName[source];
    if (!sourceConfig || !sourceConfig.color) {
        return '#333333'; // fallback
    }

    const baseColor = parseColor(sourceConfig.color);
    // Darken for better readability on white background
    const darkenedColor = darkenColor(baseColor, 0.6);
    // Fade based on weight (lower weight = more faded toward white)
    const fadedColor = fadeColor(darkenedColor, weight);
    return colorToHex(fadedColor);
}

function getSourceColor(source) {
    const sourceConfig = State.sourcesByName[source];
    if (!sourceConfig || !sourceConfig.color) {
        return '#333333';
    }
    return colorToHex(parseColor(sourceConfig.color));
}

// =============================================================================
// WebSocket Communication
// =============================================================================

function connect() {
    updateConnectionStatus('connecting');

    State.ws = new WebSocket(State.serverUrl);

    State.ws.onopen = () => {
        State.connected = true;
        updateConnectionStatus('connected');
        // First get configuration, then find roots
        getConfiguration();
    };

    State.ws.onclose = () => {
        State.connected = false;
        updateConnectionStatus('disconnected');
        setTimeout(connect, 3000);
    };

    State.ws.onerror = (err) => {
        console.error('WebSocket error:', err);
        updateConnectionStatus('error');
    };

    State.ws.onmessage = (event) => {
        try {
            const response = JSON.parse(event.data);
            handleResponse(response);
        } catch (e) {
            console.error('Failed to parse response:', e);
        }
    };
}

function sendAction(action, callback = null) {
    if (!State.connected) {
        console.warn('Not connected');
        return;
    }

    // Generate a unique request ID
    const requestId = 'req-' + (State.nextRequestId++);

    // Store callback with request ID
    if (callback) {
        State.pendingCallbacks[requestId] = callback;
    }

    // Add request ID to action so we can match the response
    action.requestId = requestId;

    const request = {
        op: 'eval',
        processor: '',
        args: {
            language: 'smsn',
            gremlin: JSON.stringify(action)
        }
    };

    State.ws.send(JSON.stringify(request));
}

function handleResponse(response) {
    if (response.status && response.status.code !== 200) {
        console.error('Server error:', response.status);
        setStatusMessage('Error: ' + (response.status.message || 'Unknown error'));
        return;
    }

    if (response.result && response.result.data && response.result.data.length > 0) {
        const data = JSON.parse(response.result.data[0]);

        // Look up callback by request ID
        const requestId = data.requestId;
        const callback = requestId ? State.pendingCallbacks[requestId] : null;
        if (requestId) {
            delete State.pendingCallbacks[requestId];
        }

        if (callback) {
            callback(data);
        } else if (data.view) {
            State.view = data.view;
            State.rootId = data.root || (data.view.id);
            // Expand root by default and select root node
            State.expandedNodes.add(State.rootId);
            State.selectedId = State.rootId;
            // Also sync to active pane
            const pane = State.panes[State.activePane];
            pane.view = State.view;
            pane.rootId = State.rootId;
            pane.expandedNodes.add(State.rootId);
            pane.selectedId = State.rootId;
            render();
            updateToolbar();
            focusTreeContainer();
            setStatusMessage('View loaded');
        }
    }
}

// =============================================================================
// Server Actions
// =============================================================================

function getConfiguration() {
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.GetConfiguration'
    }, (data) => {
        if (data.configuration) {
            const config = JSON.parse(data.configuration);
            State.config = config;

            // Build source lookup
            if (config.sources) {
                State.sourcesByName = {};
                config.sources.forEach(s => {
                    State.sourcesByName[s.name] = s;
                });

                // Update source dropdown and legend
                updateSourceOptions();
                updateSourceLegend();
                updateSourceFilterDropdown();
            }
        }
        // Don't auto-load anything - wait for user action
        setStatusMessage('Ready. Press / to search or C-c C-f to find roots.');
        // Clear the "Connecting..." message from tree containers
        document.getElementById('tree-container-0').innerHTML = '<div class="loading">Press / to search</div>';
    });
}

function getView(rootId, height = null) {
    const pane = getPane();
    const viewHeight = height || pane.viewDepth || 2;
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.GetView',
        root: rootId,
        height: viewHeight,
        filter: getFilterForServer(),
        style: pane.viewStyle
    });
    updateToolbar();
}

function search(query) {
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.Search',
        query: query,
        queryType: 'FullText',
        height: 1,
        filter: getFilterForServer(),
        titleCutoff: 100
    });
}

function searchInPane(query, paneIndex) {
    const pane = State.panes[paneIndex];
    // Save current view to history before showing search results
    if (pane.rootId) {
        const entry = createHistoryEntry(pane);
        if (entry) pane.history.push(entry);
        pane.forwardHistory.length = 0;
    }
    // Save the search query so we can re-execute it when navigating back
    pane.lastSearchQuery = query;
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.Search',
        query: query,
        queryType: 'FullText',
        height: 1,
        filter: getFilterForServer(),
        titleCutoff: 100
    }, (data) => {
        if (data.view) {
            pane.view = data.view;
            // Override root title to include search term
            pane.view.title = `Search results for "${query}"`;
            // Use a special marker so we know this is a search result
            pane.rootId = SEARCH_RESULT_PREFIX + query;
            // Expand root and select root for search results
            pane.expandedNodes.add(data.view.id);
            pane.selectedId = data.view.id;
            if (paneIndex === State.activePane) {
                State.view = pane.view;
                State.rootId = pane.rootId;
                State.expandedNodes.add(data.view.id);
                State.selectedId = data.view.id;
            }
            render();
            updateToolbar();
            focusTreeContainer();
            setStatusMessage(`Found ${countNodes(data.view)} results`);
        }
    });
}

function findRoots() {
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.FindRoots',
        height: 2,
        filter: getFilterForServer()
    });
}

// =============================================================================
// Undo Support
// =============================================================================

function pushUndo(operation) {
    State.undoStack.push(operation);
    // Limit stack size
    if (State.undoStack.length > State.maxUndoSize) {
        State.undoStack.shift();
    }
}

function undo() {
    if (State.undoStack.length === 0) {
        setStatusMessage('Nothing to undo');
        return;
    }

    const op = State.undoStack.pop();

    switch (op.type) {
        case 'setProperty':
            // Restore the old value
            setPropertyNoUndo(op.nodeId, op.propertyName, op.oldValue, () => {
                refreshView();
                setStatusMessage(`Undo: ${op.propertyName} restored`);
            });
            break;

        case 'delete':
            // Re-add the deleted note as a child of its parent
            // This requires using UpdateView with wiki format
            const wikiContent = `* :${op.parentId}:\n    * ${op.note.title}`;
            sendAction({
                action: 'net.fortytwo.smsn.server.actions.UpdateView',
                root: op.parentId,
                view: wikiContent,
                viewFormat: 'wiki',
                height: 2,
                filter: getFilterForServer(),
                style: 'forward'
            }, () => {
                refreshView();
                setStatusMessage(`Undo: "${op.note.title}" restored`);
            });
            break;

        case 'create':
            // Delete the created note
            // Note: this is tricky since we'd need to know the new note's ID
            setStatusMessage('Undo create not yet supported');
            break;

        case 'move':
            // Move the note back to its original position
            // This would require re-implementing the move in reverse
            setStatusMessage('Undo move not yet supported');
            break;

        default:
            setStatusMessage('Unknown undo operation');
    }
}

// Version of setProperty that doesn't add to undo stack (used by undo itself)
function setPropertyNoUndo(nodeId, propertyName, value, callback = null) {
    let sendValue = value;
    if ((propertyName === 'weight' || propertyName === 'priority') && typeof value === 'number') {
        if (value === 1) {
            sendValue = 0.9999999;
        } else if (value === 0) {
            sendValue = 0.0000001;
        } else if (Number.isInteger(value)) {
            sendValue = value + 0.0000001;
        }
    }
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.SetProperties',
        id: nodeId,
        name: propertyName,
        value: sendValue,
        filter: getFilterForServer()
    }, callback);
}

function setProperty(nodeId, propertyName, value, callback = null, oldValue = null) {
    // Record undo info if oldValue is provided
    if (oldValue !== null) {
        pushUndo({
            type: 'setProperty',
            nodeId: nodeId,
            propertyName: propertyName,
            oldValue: oldValue
        });
    }

    // Ensure weight and priority are always sent as floats (not integers)
    // to avoid ClassCastException on the server (expects Double, not Integer)
    let sendValue = value;
    if ((propertyName === 'weight' || propertyName === 'priority') && typeof value === 'number') {
        // Force a decimal representation by adding tiny epsilon away from boundaries
        if (value === 1) {
            sendValue = 0.9999999;  // Just under 1.0
        } else if (value === 0) {
            sendValue = 0.0000001;  // Just over 0.0
        } else if (Number.isInteger(value)) {
            sendValue = value + 0.0000001;
        }
    }
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.SetProperties',
        id: nodeId,
        name: propertyName,
        value: sendValue,
        filter: getFilterForServer()
    }, callback);
}

function updateView(rootId, viewContent, height = 2, preserveSelection = false) {
    // Remember the current view root and selection so we can restore after refresh
    const currentRootId = State.rootId;
    const currentSelectedId = preserveSelection ? State.selectedId : null;

    sendAction({
        action: 'net.fortytwo.smsn.server.actions.UpdateView',
        root: rootId,
        view: viewContent,
        viewFormat: 'wiki',
        height: height,
        filter: getFilterForServer(),
        style: 'forward'
    }, (data) => {
        if (data.view) {
            // If we updated a subtree, refresh the original view to show changes
            if (currentRootId && currentRootId !== rootId) {
                refreshViewPreserveSelection(currentSelectedId);
                setStatusMessage('Updated');
            } else {
                // We updated the root itself
                State.view = data.view;
                State.rootId = data.root || data.view.id;
                State.expandedNodes.add(State.rootId);
                State.selectedId = currentSelectedId || State.rootId;
                const pane = State.panes[State.activePane];
                pane.view = State.view;
                pane.rootId = State.rootId;
                pane.expandedNodes.add(State.rootId);
                pane.selectedId = State.selectedId;
                render();
                focusTreeContainer();
                setStatusMessage('Updated');
            }
        }
    });
}

// Convert a node and its children to wiki format recursively
function nodeToWiki(node, indent = 0) {
    const prefix = '    '.repeat(indent);
    let wiki = `${prefix}* ${node.title}\n`;
    wiki += `${prefix}    :${node.id}:\n`;
    if (node.children && node.children.length > 0) {
        for (const child of node.children) {
            wiki += nodeToWiki(child, indent + 1);
        }
    }
    return wiki;
}

// Push the current view to the server (C-c p in smsn-mode)
function pushViewToServer() {
    const pane = getPane();
    if (!pane.view || !pane.rootId) {
        setStatusMessage('No view to push');
        return;
    }

    // Convert the current view to wiki format
    let wikiContent = nodeToWiki(pane.view);

    sendAction({
        action: 'net.fortytwo.smsn.server.actions.UpdateView',
        root: pane.rootId,
        view: wikiContent,
        viewFormat: 'wiki',
        height: pane.height || 2,
        filter: getFilterForServer(),
        style: pane.viewStyle || 'forward'
    }, (data) => {
        if (data.view) {
            pane.view = data.view;
            pane.rootId = data.root || data.view.id;
            pane.expandedNodes.add(pane.rootId);
            if (State.activePane === 0) {
                State.view = data.view;
                State.rootId = pane.rootId;
                State.expandedNodes.add(State.rootId);
            }
            render();
            focusTreeContainer();
            setStatusMessage('View pushed to server');
        }
    });
}

// =============================================================================
// Navigation
// =============================================================================

function visitTarget(nodeId) {
    const pane = getPane();
    if (pane.rootId) {
        const entry = createHistoryEntry(pane);
        if (entry) {
            pane.history.push(entry);
            console.log('visitTarget: pushed to history, now length:', pane.history.length);
        }
        pane.forwardHistory.length = 0;  // Clear forward history
    }
    if (State.activePane === 0) {
        getView(nodeId);
    } else {
        getViewForPane(nodeId, State.activePane);
    }
}

function popView() {
    const pane = getPane();
    if (pane.history.length > 0) {
        if (pane.rootId) {
            const entry = createHistoryEntry(pane);
            if (entry) pane.forwardHistory.push(entry);
        }
        const prevEntry = pane.history.pop();
        navigateToEntry(prevEntry, State.activePane);
    }
    updateToolbar();
}

function forwardView() {
    const pane = getPane();
    if (pane.forwardHistory.length > 0) {
        if (pane.rootId) {
            const entry = createHistoryEntry(pane);
            if (entry) pane.history.push(entry);
        }
        const nextEntry = pane.forwardHistory.pop();
        navigateToEntry(nextEntry, State.activePane);
    }
    updateToolbar();
}

// Navigate to a history entry (object with id, title, etc.)
function navigateToEntry(entry, paneIndex) {
    if (!entry) return;

    // Handle both old format (just ID string) and new format (object)
    if (typeof entry === 'string') {
        navigateToId(entry, paneIndex);
    } else {
        navigateToHistoryEntry(entry, paneIndex);
    }
}

function navigateToId(targetId, paneIndex) {
    // Check if this is a search result marker
    if (targetId && targetId.startsWith(SEARCH_RESULT_PREFIX)) {
        const query = targetId.substring(SEARCH_RESULT_PREFIX.length);
        // Re-execute the search
        reExecuteSearch(query, paneIndex);
    } else {
        // Normal view navigation
        if (paneIndex === 0) {
            getView(targetId);
        } else {
            getViewForPane(targetId, paneIndex);
        }
    }
}

function reExecuteSearch(query, paneIndex) {
    const pane = State.panes[paneIndex];
    pane.lastSearchQuery = query;
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.Search',
        query: query,
        queryType: 'FullText',
        height: 1,
        filter: getFilterForServer(),
        titleCutoff: 100
    }, (data) => {
        if (data.view) {
            pane.view = data.view;
            // Override root title to include search term
            pane.view.title = `Search results for "${query}"`;
            pane.rootId = SEARCH_RESULT_PREFIX + query;
            // Expand root and select root for search results
            pane.expandedNodes.add(data.view.id);
            pane.selectedId = data.view.id;
            if (paneIndex === State.activePane) {
                State.view = pane.view;
                State.rootId = pane.rootId;
                State.expandedNodes.add(data.view.id);
                State.selectedId = data.view.id;
            }
            render();
            updateToolbar();
            focusTreeContainer();
            setStatusMessage(`Search: "${query}"`);
        }
    });
}

function refreshView() {
    const pane = getPane();
    if (pane.rootId) {
        // Handle refresh of search results
        if (pane.rootId.startsWith(SEARCH_RESULT_PREFIX)) {
            const query = pane.rootId.substring(SEARCH_RESULT_PREFIX.length);
            reExecuteSearch(query, State.activePane);
        } else if (State.activePane === 0) {
            getView(pane.rootId);
        } else {
            getViewForPane(pane.rootId, State.activePane);
        }
    } else {
        findRoots();
    }
}

function refreshViewPreserveSelection(selectedId) {
    const pane = getPane();
    if (pane.rootId) {
        const viewHeight = pane.viewDepth || 2;
        sendAction({
            action: 'net.fortytwo.smsn.server.actions.GetView',
            root: pane.rootId,
            height: viewHeight,
            filter: getFilterForServer(),
            style: pane.viewStyle
        }, (data) => {
            if (data.view) {
                const foundNode = findNodeById(data.view, selectedId);
                State.view = data.view;
                State.rootId = data.root || data.view.id;
                State.expandedNodes.add(State.rootId);
                // Restore the previous selection if the node still exists
                if (selectedId && foundNode) {
                    State.selectedId = selectedId;
                } else {
                    State.selectedId = State.rootId;
                }
                const pane = State.panes[State.activePane];
                pane.view = State.view;
                pane.rootId = State.rootId;
                pane.expandedNodes.add(State.rootId);
                pane.selectedId = State.selectedId;
                render();
                updateToolbar();
                focusTreeContainer();
            }
        });
    }
}

function setViewStyle(style) {
    const pane = getPane();
    pane.viewStyle = style;
    if (State.activePane === 0) {
        State.viewStyle = style;
    }
    updateToolbar();
    if (pane.rootId) {
        if (State.activePane === 0) {
            getView(pane.rootId);
        } else {
            getViewForPane(pane.rootId, State.activePane);
        }
    }
}

function updateToolbar() {
    // Update both panes' toolbars
    for (let i = 0; i < 2; i++) {
        const pane = State.panes[i];
        const backBtn = document.getElementById(`btn-back-${i}`);
        const fwdBtn = document.getElementById(`btn-forward-${i}`);
        const fwdViewBtn = document.getElementById(`btn-forward-view-${i}`);
        const bkViewBtn = document.getElementById(`btn-backward-view-${i}`);
        const historyLink = document.getElementById(`link-history-${i}`);

        if (backBtn) backBtn.disabled = pane.history.length === 0;
        if (fwdBtn) fwdBtn.disabled = pane.forwardHistory.length === 0;
        if (fwdViewBtn) fwdViewBtn.classList.toggle('active', pane.viewStyle === 'forward');
        if (bkViewBtn) bkViewBtn.classList.toggle('active', pane.viewStyle === 'backward');
        // History link is disabled when there's no view loaded
        if (historyLink) historyLink.classList.toggle('disabled', !pane.rootId);
    }

    document.getElementById('btn-split').classList.toggle('active', State.splitView);
}

function toggleSplit() {
    State.splitView = !State.splitView;
    document.getElementById('panes-container').classList.toggle('split-view', State.splitView);

    // If opening split and pane 1 has no view, copy current view
    if (State.splitView && !State.panes[1].view) {
        State.panes[1].rootId = State.panes[0].rootId;
        State.panes[1].view = JSON.parse(JSON.stringify(State.panes[0].view));
        State.panes[1].viewStyle = State.panes[0].viewStyle;
        State.panes[1].lastSearchQuery = State.panes[0].lastSearchQuery;
        renderPane(1);
        // Focus the newly created right pane and its search bar
        setActivePane(1);
        const searchInput = document.getElementById('search-input-1');
        if (searchInput) searchInput.focus();
    }

    updateToolbar();
}

function toggleWrapTitles() {
    State.wrapTitles = !State.wrapTitles;
    document.body.classList.toggle('no-wrap-titles', !State.wrapTitles);
    setStatusMessage(State.wrapTitles ? 'Line wrap: on' : 'Line wrap: off (scroll for long titles)');
}

function toggleLineNumbers() {
    State.showLineNumbers = !State.showLineNumbers;
    render();
    setStatusMessage(State.showLineNumbers ? 'Line numbers: on' : 'Line numbers: off');
}

function gotoLineNumber(lineNum) {
    const container = document.getElementById(`tree-container-${State.activePane}`);
    const node = container.querySelector(`.tree-node[data-line="${lineNum}"]`);

    // Check if the node is visible (not inside a collapsed ancestor)
    const isVisible = node && !node.closest('.children.collapsed');

    if (node && isVisible) {
        const nodeId = node.dataset.id;
        selectNode(nodeId);
        node.scrollIntoView({ block: 'center' });
        setStatusMessage(`Line ${lineNum}`);
        focusTreeContainer();
    } else {
        // Line might be in a collapsed section - try to find it
        // First, count total lines in the fully expanded view
        const pane = State.panes[State.activePane];
        const totalLines = countTotalLines(pane.view);

        if (lineNum > totalLines) {
            setStatusMessage(`Line ${lineNum} out of range (max: ${totalLines})`);
            focusTreeContainer();
        } else {
            // The line exists but is collapsed - expand to reveal it
            expandToLine(lineNum);
        }
    }
}

function countTotalLines(node) {
    if (!node) return 0;
    let count = 1;  // Count this node
    if (node.children) {
        for (const child of node.children) {
            count += countTotalLines(child);
        }
    }
    return count;
}

function expandToLine(targetLine) {
    const pane = State.panes[State.activePane];
    if (!pane.view) {
        setStatusMessage(`No view to expand`);
        focusTreeContainer();
        return;
    }

    // Find path to the target line and expand all ancestors
    const path = findPathToLine(pane.view, targetLine, { value: 1 });
    if (path && path.length > 0) {
        // Expand all nodes in the path except the target
        for (let i = 0; i < path.length - 1; i++) {
            pane.expandedNodes.add(path[i].id);
        }
        // Also sync to State if this is active pane
        if (State.activePane === 0 || State.activePane === 1) {
            State.expandedNodes = pane.expandedNodes;
        }
        // Re-render and then select the target
        render();

        // After render, the line should now be visible - select it
        const targetNode = path[path.length - 1];
        selectNode(targetNode.id);

        // Scroll to it
        const container = document.getElementById(`tree-container-${State.activePane}`);
        const nodeEl = container.querySelector(`.tree-node[data-id="${targetNode.id}"]`);
        if (nodeEl) {
            nodeEl.scrollIntoView({ block: 'center' });
        }
        setStatusMessage(`Line ${targetLine}`);
        focusTreeContainer();
    } else {
        setStatusMessage(`Could not find line ${targetLine}`);
        focusTreeContainer();
    }
}

function findPathToLine(node, targetLine, counter, path = []) {
    if (!node) return null;

    const currentLine = counter.value++;
    path.push(node);

    if (currentLine === targetLine) {
        return path.slice();  // Return a copy
    }

    if (node.children) {
        for (const child of node.children) {
            const result = findPathToLine(child, targetLine, counter, path);
            if (result) return result;
        }
    }

    path.pop();
    return null;
}

function setActivePane(index) {
    if (State.activePane === index) return;

    // Save current state to old pane
    syncStateToPane(State.activePane);

    State.activePane = index;

    // Load state from new pane
    syncPaneToState(index);

    // Update visual - use pane-wrapper class
    document.querySelectorAll('.pane-wrapper').forEach((el, i) => {
        el.classList.toggle('active-pane', i === index);
    });

    updateToolbar();
}

function syncStateToPane(paneIndex) {
    const pane = State.panes[paneIndex];
    pane.rootId = State.rootId;
    pane.view = State.view;
    pane.selectedId = State.selectedId;
    pane.expandedNodes = State.expandedNodes;
    pane.history = State.history;
    pane.forwardHistory = State.forwardHistory;
    pane.viewStyle = State.viewStyle;
}

function syncPaneToState(paneIndex) {
    const pane = State.panes[paneIndex];
    State.rootId = pane.rootId;
    State.view = pane.view;
    State.selectedId = pane.selectedId;
    State.expandedNodes = pane.expandedNodes;
    State.history = pane.history;
    State.forwardHistory = pane.forwardHistory;
    State.viewStyle = pane.viewStyle;
}

function selectNode(nodeId) {
    State.selectedId = nodeId;
    getPane().selectedId = nodeId;
    render();
}

function focusTreeContainer() {
    // Focus the active pane's tree container to enable keyboard navigation
    const container = document.getElementById(`tree-container-${State.activePane}`);
    if (container) {
        container.focus();
    }
}

function toggleExpand(nodeId) {
    const pane = getPane();
    if (pane.expandedNodes.has(nodeId)) {
        pane.expandedNodes.delete(nodeId);
    } else {
        pane.expandedNodes.add(nodeId);
    }
    // Keep State in sync
    State.expandedNodes = pane.expandedNodes;
    render();
}

function toggleExpandPane(nodeId, paneIndex) {
    const pane = State.panes[paneIndex];
    if (pane.expandedNodes.has(nodeId)) {
        pane.expandedNodes.delete(nodeId);
    } else {
        pane.expandedNodes.add(nodeId);
    }
    // Also sync to active state if this is the active pane
    if (paneIndex === State.activePane) {
        State.expandedNodes = pane.expandedNodes;
    }
    render();
}

function handleNodeClickPane(nodeId, paneIndex) {
    if (State.editingNodeId) return;

    // If clicking on a different pane, activate it
    if (paneIndex !== State.activePane) {
        setActivePane(paneIndex);
    }

    const pane = State.panes[paneIndex];
    if (pane.selectedId === nodeId) {
        // Double-click behavior: push view
        visitTargetPane(nodeId, paneIndex);
    } else {
        // Single click: select
        selectNodePane(nodeId, paneIndex);
    }
}

function selectNodePane(nodeId, paneIndex) {
    const pane = State.panes[paneIndex];
    pane.selectedId = nodeId;
    if (paneIndex === State.activePane) {
        State.selectedId = nodeId;
    }
    render();
}

function visitTargetPane(nodeId, paneIndex) {
    const pane = State.panes[paneIndex];
    if (pane.rootId) {
        const entry = createHistoryEntry(pane);
        if (entry) pane.history.push(entry);
        pane.forwardHistory = [];
    }
    getViewForPane(nodeId, paneIndex);
}

function getViewForPane(rootId, paneIndex, height = null) {
    const pane = State.panes[paneIndex];
    const viewHeight = height || pane.viewDepth || 2;
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.GetView',
        root: rootId,
        height: viewHeight,
        filter: getFilterForServer(),
        style: pane.viewStyle
    }, (data) => {
        if (data.view) {
            pane.view = data.view;
            pane.rootId = data.root || data.view.id;
            // Expand root by default and select root node
            pane.expandedNodes.add(pane.rootId);
            pane.selectedId = pane.rootId;
            if (paneIndex === State.activePane) {
                State.view = pane.view;
                State.rootId = pane.rootId;
                State.expandedNodes.add(State.rootId);
                State.selectedId = State.rootId;
            }
            render();
            focusTreeContainer();
            setStatusMessage('View loaded');
        }
    });
    updateToolbar();
}

function popViewPane(paneIndex) {
    const pane = State.panes[paneIndex];
    if (pane.history.length > 0) {
        if (pane.rootId) {
            const entry = createHistoryEntry(pane);
            if (entry) pane.forwardHistory.push(entry);
        }
        const prevEntry = pane.history.pop();
        navigateToEntry(prevEntry, paneIndex);
    }
    updateToolbar();
}

function forwardViewPane(paneIndex) {
    const pane = State.panes[paneIndex];
    if (pane.forwardHistory.length > 0) {
        if (pane.rootId) {
            const entry = createHistoryEntry(pane);
            if (entry) pane.history.push(entry);
        }
        const nextEntry = pane.forwardHistory.pop();
        navigateToEntry(nextEntry, paneIndex);
    }
    updateToolbar();
}

function setViewStylePane(style, paneIndex) {
    const pane = State.panes[paneIndex];
    pane.viewStyle = style;
    updateToolbar();
    if (pane.rootId) {
        getViewForPane(pane.rootId, paneIndex);
    }
}

function refreshViewPane(paneIndex) {
    const pane = State.panes[paneIndex];
    if (pane.rootId) {
        getViewForPane(pane.rootId, paneIndex);
    }
}

function setViewDepth(paneIndex, depth) {
    const pane = State.panes[paneIndex];
    pane.viewDepth = parseInt(depth, 10);
    if (pane.rootId) {
        getViewForPane(pane.rootId, paneIndex, pane.viewDepth);
    }
}

function gotoAlias() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }

    const pane = getPane();
    const node = findNodeById(pane.view, State.selectedId);
    if (!node) {
        setStatusMessage('Note not found');
        return;
    }

    // Check if node has a shortcut or alias - navigate to it
    const shortcutOrAlias = node.shortcut || node.alias;
    if (shortcutOrAlias) {
        searchShortcut(shortcutOrAlias);
        return;
    }

    // Check if title looks like a URL - open in new tab
    const title = node.title || '';
    const urlPattern = /^(https?:\/\/|www\.)/i;
    if (urlPattern.test(title)) {
        const url = title.startsWith('www.') ? 'https://' + title : title;
        window.open(url, '_blank');
        setStatusMessage('Opened URL in new tab');
        return;
    }

    // Check if title contains a URL
    const urlMatch = title.match(/(https?:\/\/[^\s]+)/i);
    if (urlMatch) {
        window.open(urlMatch[1], '_blank');
        setStatusMessage('Opened URL in new tab');
        return;
    }

    setStatusMessage('No alias or URL to visit');
}

function searchShortcut(shortcut) {
    sendAction({
        action: 'net.fortytwo.smsn.server.actions.Search',
        query: shortcut,
        queryType: 'Shortcut',
        height: 2,
        filter: getFilterForServer()
    }, (data) => {
        if (data.view && data.view.children && data.view.children.length > 0) {
            // Navigate to the first result
            const targetId = data.view.children[0].id;
            visitTarget(targetId);
            setStatusMessage('Jumped to: ' + shortcut);
        } else {
            setStatusMessage('Shortcut not found: ' + shortcut);
        }
    });
}

// =============================================================================
// Editing Functions
// =============================================================================

function autoResizeTextarea(textarea) {
    // Reset to minimum height to get accurate scrollHeight
    textarea.style.height = '0';
    // Set height to scrollHeight to fit content (scrollHeight includes padding)
    textarea.style.height = textarea.scrollHeight + 'px';
    // Scroll to top so user sees beginning of content
    textarea.scrollTop = 0;
}

function startEditTitle(nodeId) {
    State.editingNodeId = nodeId;
    render();

    setTimeout(() => {
        const input = document.getElementById('edit-title-input');
        if (input) {
            input.focus();
            input.select();
            // Trigger auto-resize to fit initial content
            autoResizeTextarea(input);
        }
    }, 0);
}

function cancelEditTitle() {
    State.editingNodeId = null;
    render();
}

function saveEditTitle() {
    const input = document.getElementById('edit-title-input');
    if (!input || !State.editingNodeId) return;

    const newTitle = input.value.trim();
    if (!newTitle) {
        setStatusMessage('Title cannot be empty');
        return;
    }

    const nodeId = State.editingNodeId;
    State.editingNodeId = null;

    setProperty(nodeId, 'title', newTitle, () => {
        const node = findNodeById(State.view, nodeId);
        if (node) {
            node.title = newTitle;
        }
        render();
        setStatusMessage('Title updated');
    });
}

function showProperties() {
    if (!State.selectedId) return;

    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    // Set meta line with ID and created timestamp
    const metaEl = document.getElementById('prop-meta');
    let metaText = `Id: ${node.id}`;
    if (node.created) {
        const date = new Date(node.created);
        const dateStr = date.toLocaleDateString();
        const timeStr = date.toLocaleTimeString();
        metaText += ` | Created: ${dateStr} at ${timeStr} (${node.created})`;
    }
    metaEl.textContent = metaText;

    const titleInput = document.getElementById('prop-title');
    titleInput.value = node.title || '';
    document.getElementById('prop-weight').value = node.weight || 0.5;
    document.getElementById('prop-weight-value').textContent = (node.weight || 0.5).toFixed(1);
    document.getElementById('prop-priority').value = node.priority || 0;
    document.getElementById('prop-priority-value').textContent = node.priority ? node.priority.toFixed(1) : '-';
    selectSource(node.source || 'private');
    document.getElementById('prop-alias').value = node.alias || '';
    const textInput = document.getElementById('prop-text');
    textInput.value = node.text || '';

    document.getElementById('properties-overlay').classList.add('visible');
    titleInput.focus();
    // Auto-resize textareas to fit content, position cursor at start of title
    setTimeout(() => {
        autoResizeTextarea(titleInput);
        autoResizeTextarea(textInput);
        titleInput.setSelectionRange(0, 0);
        titleInput.scrollTop = 0;
    }, 0);
}

function hideProperties() {
    document.getElementById('properties-overlay').classList.remove('visible');
}

function saveProperties() {
    if (!State.selectedId) return;

    const nodeId = State.selectedId;
    const props = {
        title: document.getElementById('prop-title').value.trim(),
        weight: parseFloat(document.getElementById('prop-weight').value),
        priority: parseFloat(document.getElementById('prop-priority').value),
        source: getSelectedSource(),
        alias: document.getElementById('prop-alias').value.trim(),
        text: document.getElementById('prop-text').value
    };

    hideProperties();

    let updates = [];  // Each entry: [name, newValue, oldValue]

    const node = findNodeById(State.view, nodeId);
    if (props.title && props.title !== node.title) {
        updates.push(['title', props.title, node.title]);
    }
    if (props.weight !== node.weight) {
        updates.push(['weight', props.weight, node.weight]);
    }
    if (props.priority !== (node.priority || 0)) {
        updates.push(['priority', props.priority, node.priority || 0]);
    }
    if (props.source !== node.source) {
        updates.push(['source', props.source, node.source]);
    }
    if (props.alias !== (node.alias || '')) {
        updates.push(['alias', props.alias || null, node.alias || null]);
    }
    if (props.text !== (node.text || '')) {
        updates.push(['text', props.text || null, node.text || null]);
    }

    if (updates.length === 0) {
        setStatusMessage('No changes');
        return;
    }

    function sendNext(index) {
        if (index >= updates.length) {
            getView(State.rootId);
            setStatusMessage('Properties updated');
            return;
        }

        const [name, value, oldValue] = updates[index];
        setProperty(nodeId, name, value, () => {
            sendNext(index + 1);
        }, oldValue);
    }

    sendNext(0);
}

function showNewNote(mode) {
    State.newNoteMode = mode;
    document.getElementById('new-note-title').textContent =
        mode === 'child' ? 'New Child Note' : 'New Sibling Note';

    // Clear/reset all fields
    document.getElementById('new-note-input').value = '';
    document.getElementById('new-note-alias').value = '';

    // Default weight to 0.5
    document.getElementById('new-note-weight').value = 0.5;
    document.getElementById('new-note-weight-value').textContent = '0.5';

    // Default priority to 0 (none)
    document.getElementById('new-note-priority').value = 0;
    document.getElementById('new-note-priority-value').textContent = '-';

    // Populate source dropdown if not already done
    const sourceSelect = document.getElementById('new-note-source');
    if (sourceSelect.options.length === 0 && State.config && State.config.sources) {
        State.config.sources.forEach(s => {
            const option = document.createElement('option');
            option.value = s.name;
            option.textContent = s.name;
            sourceSelect.appendChild(option);
        });
    }

    // Get parent's source for inheritance
    let parentSource = 'private';  // fallback default
    if (mode === 'child') {
        const parentId = State.selectedId || State.rootId;
        if (parentId) {
            const parent = findNodeById(State.view, parentId);
            if (parent && parent.source) {
                parentSource = parent.source;
            }
        }
    } else if (mode === 'sibling') {
        // For sibling, inherit from the selected node's parent
        if (State.selectedId) {
            const parentId = findParentId(State.view, State.selectedId);
            if (parentId) {
                const parent = findNodeById(State.view, parentId);
                if (parent && parent.source) {
                    parentSource = parent.source;
                }
            }
        }
    }
    sourceSelect.value = parentSource;

    // Update Create button state based on title
    updateNewNoteCreateButton();

    document.getElementById('new-note-overlay').classList.add('visible');
    document.getElementById('new-note-input').focus();
}

function updateNewNoteCreateButton() {
    const title = document.getElementById('new-note-input').value.trim();
    const createBtn = document.querySelector('#new-note-overlay .primary');
    if (createBtn) {
        createBtn.disabled = !title;
    }
}

function hideNewNote() {
    document.getElementById('new-note-overlay').classList.remove('visible');
    State.newNoteMode = null;
}

function createNewNote() {
    const title = document.getElementById('new-note-input').value.trim();
    if (!title) {
        setStatusMessage('Title cannot be empty');
        document.getElementById('new-note-input').focus();
        return;
    }

    // Collect all properties from the form
    const weight = parseFloat(document.getElementById('new-note-weight').value);
    const priority = parseFloat(document.getElementById('new-note-priority').value);
    const source = document.getElementById('new-note-source').value;
    const alias = document.getElementById('new-note-alias').value.trim();

    // Save mode before hideNewNote clears it
    const mode = State.newNoteMode;
    hideNewNote();

    // Get existing children IDs so we can identify the new note after creation
    let parentId;
    let existingChildIds = new Set();

    if (mode === 'child') {
        parentId = State.selectedId || State.rootId;
        if (!parentId) {
            setStatusMessage('No parent selected');
            return;
        }
        const parent = findNodeById(State.view, parentId);
        if (parent && parent.children) {
            parent.children.forEach(c => existingChildIds.add(c.id));
        }
    } else if (mode === 'sibling') {
        if (!State.selectedId || State.selectedId === State.rootId) {
            setStatusMessage('Cannot add sibling to root');
            return;
        }
        parentId = findParentId(State.view, State.selectedId);
        if (!parentId) {
            setStatusMessage('Could not find parent');
            return;
        }
        const parent = findNodeById(State.view, parentId);
        if (parent && parent.children) {
            parent.children.forEach(c => existingChildIds.add(c.id));
        }
    }

    // Build wiki content
    const parent = findNodeById(State.view, parentId);
    let wikiContent = '';

    if (mode === 'child') {
        if (parent && parent.children) {
            for (const child of parent.children) {
                wikiContent += `* ${child.title}\n    :${child.id}:\n`;
            }
        }
        wikiContent += `* ${title}\n`;
    } else if (mode === 'sibling') {
        if (parent && parent.children) {
            for (const child of parent.children) {
                wikiContent += `* ${child.title}\n    :${child.id}:\n`;
                if (child.id === State.selectedId) {
                    wikiContent += `* ${title}\n`;
                }
            }
        }
    }

    // Create the note and then set its properties
    createNoteWithProperties(parentId, wikiContent, existingChildIds, {
        weight, priority, source, alias
    });
}

function createNoteWithProperties(parentId, wikiContent, existingChildIds, props) {
    const currentRootId = State.rootId;

    sendAction({
        action: 'net.fortytwo.smsn.server.actions.UpdateView',
        root: parentId,
        view: wikiContent,
        viewFormat: 'wiki',
        height: 2,
        filter: getFilterForServer(),
        style: 'forward'
    }, (data) => {
        if (data.view) {
            // Find the newly created note by looking for a child that wasn't there before
            let newNoteId = null;
            if (data.view.children) {
                for (const child of data.view.children) {
                    if (!existingChildIds.has(child.id)) {
                        newNoteId = child.id;
                        break;
                    }
                }
            }

            // Set properties on the new note if we found it and have non-default values
            if (newNoteId) {
                setNewNoteProperties(newNoteId, props, () => {
                    // Refresh the original view to show changes
                    if (currentRootId && currentRootId !== parentId) {
                        refreshView();
                    } else {
                        State.view = data.view;
                        State.rootId = data.root || data.view.id;
                        State.expandedNodes.add(State.rootId);
                        State.selectedId = newNoteId;  // Select the new note
                        const pane = State.panes[State.activePane];
                        pane.view = State.view;
                        pane.rootId = State.rootId;
                        pane.expandedNodes.add(State.rootId);
                        pane.selectedId = newNoteId;
                        render();
                        focusTreeContainer();
                    }
                    setStatusMessage('Note created');
                });
            } else {
                // Couldn't find new note, just refresh
                if (currentRootId && currentRootId !== parentId) {
                    refreshView();
                }
                setStatusMessage('Note created');
            }
        }
    });
}

function setNewNoteProperties(noteId, props, callback) {
    const updates = [];

    // Only set non-default properties
    if (props.weight !== 0.5) {
        updates.push(['weight', props.weight]);
    }
    if (props.priority > 0) {
        updates.push(['priority', props.priority]);
    }
    if (props.source && props.source !== 'private') {
        updates.push(['source', props.source]);
    }
    if (props.alias) {
        updates.push(['shortcut', props.alias]);
    }

    if (updates.length === 0) {
        callback();
        return;
    }

    function sendNext(index) {
        if (index >= updates.length) {
            callback();
            return;
        }
        const [name, value] = updates[index];
        setPropertyNoUndo(noteId, name, value, () => {
            sendNext(index + 1);
        });
    }

    sendNext(0);
}

function showConfirmDelete() {
    if (!State.selectedId) return;

    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    const childCount = node.numberOfChildren || (node.children ? node.children.length : 0);
    let message = `Remove "${node.title}" from parent?`;
    if (childCount > 0) {
        message += `<br><br><span style="color: var(--muted-color)">This note has ${childCount} children. They will remain in the graph but won't appear in this view.</span>`;
    }

    document.getElementById('confirm-message').innerHTML = message;
    document.getElementById('confirm-overlay').classList.add('visible');
    // Focus the Remove button so Enter confirms by default
    document.getElementById('confirm-remove-btn').focus();
}

function hideConfirm() {
    document.getElementById('confirm-overlay').classList.remove('visible');
}

function confirmDelete() {
    hideConfirm();

    if (!State.selectedId || State.selectedId === State.rootId) {
        setStatusMessage('Cannot delete root');
        return;
    }

    const parentId = findParentId(State.view, State.selectedId);
    if (!parentId) {
        setStatusMessage('Could not find parent');
        return;
    }

    // Record undo info before deletion
    const deletedNode = findNodeById(State.view, State.selectedId);
    if (deletedNode) {
        pushUndo({
            type: 'delete',
            parentId: parentId,
            note: { id: deletedNode.id, title: deletedNode.title }
        });
    }

    const parent = findNodeById(State.view, parentId);
    let wikiContent = '';

    if (parent && parent.children) {
        for (const child of parent.children) {
            if (child.id !== State.selectedId) {
                wikiContent += `* ${child.title}\n    :${child.id}:\n`;
            }
        }
    }

    State.selectedId = null;
    updateView(parentId, wikiContent, 2);
    setStatusMessage('Note removed from parent');
}

// =============================================================================
// Cut/Copy/Paste (C-w, M-w, C-y)
// =============================================================================

function cutNote() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }
    if (State.selectedId === State.rootId) {
        setStatusMessage('Cannot cut root note');
        return;
    }

    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    State.clipboard = {
        id: node.id,
        title: node.title,
        isCut: true
    };
    setStatusMessage(`Cut: "${node.title}"`);
}

function copyNote() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }

    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    State.clipboard = {
        id: node.id,
        title: node.title,
        isCut: false
    };
    setStatusMessage(`Copied: "${node.title}"`);
}

function pasteNote() {
    if (!State.clipboard) {
        setStatusMessage('Clipboard is empty');
        return;
    }

    const targetId = State.selectedId || State.rootId;
    if (!targetId) {
        setStatusMessage('No target for paste');
        return;
    }

    const target = findNodeById(State.view, targetId);
    if (!target) return;

    // Build wiki content: existing children + pasted note
    let wikiContent = '';
    if (target.children) {
        for (const child of target.children) {
            wikiContent += `* ${child.title}\n    :${child.id}:\n`;
        }
    }
    // Add the pasted note as a child
    wikiContent += `* ${State.clipboard.title}\n    :${State.clipboard.id}:\n`;

    // If it was a cut operation, remove from original parent
    if (State.clipboard.isCut) {
        const originalParentId = findParentId(State.view, State.clipboard.id);
        if (originalParentId && originalParentId !== targetId) {
            // Remove from original parent first
            const originalParent = findNodeById(State.view, originalParentId);
            let removeWiki = '';
            if (originalParent && originalParent.children) {
                for (const child of originalParent.children) {
                    if (child.id !== State.clipboard.id) {
                        removeWiki += `* ${child.title}\n    :${child.id}:\n`;
                    }
                }
            }
            // Update original parent to remove the cut note
            updateView(originalParentId, removeWiki, 2);
        }
    }

    // Update target with pasted note
    updateView(targetId, wikiContent, 2);
    const action = State.clipboard.isCut ? 'Moved' : 'Linked';
    setStatusMessage(`${action}: "${State.clipboard.title}" under "${target.title}"`);

    // Clear clipboard if it was a cut
    if (State.clipboard.isCut) {
        State.clipboard = null;
    }
}

function pasteNoteAsSibling() {
    if (!State.clipboard) {
        setStatusMessage('Clipboard is empty');
        return;
    }

    if (!State.selectedId || State.selectedId === State.rootId) {
        setStatusMessage('Select a sibling target (not root)');
        return;
    }

    const parentId = findParentId(State.view, State.selectedId);
    if (!parentId) {
        setStatusMessage('Cannot find parent');
        return;
    }

    const parent = findNodeById(State.view, parentId);
    if (!parent) return;

    // Build wiki content: existing children with pasted note inserted after selected
    let wikiContent = '';
    const selectedIndex = parent.children ? parent.children.findIndex(c => c.id === State.selectedId) : -1;

    if (parent.children) {
        for (let i = 0; i < parent.children.length; i++) {
            const child = parent.children[i];
            wikiContent += `* ${child.title}\n    :${child.id}:\n`;
            // Insert pasted note after the selected sibling
            if (i === selectedIndex) {
                wikiContent += `* ${State.clipboard.title}\n    :${State.clipboard.id}:\n`;
            }
        }
    }

    // If it was a cut operation, remove from original parent
    if (State.clipboard.isCut) {
        const originalParentId = findParentId(State.view, State.clipboard.id);
        if (originalParentId && originalParentId !== parentId) {
            const originalParent = findNodeById(State.view, originalParentId);
            let removeWiki = '';
            if (originalParent && originalParent.children) {
                for (const child of originalParent.children) {
                    if (child.id !== State.clipboard.id) {
                        removeWiki += `* ${child.title}\n    :${child.id}:\n`;
                    }
                }
            }
            updateView(originalParentId, removeWiki, 2);
        }
    }

    updateView(parentId, wikiContent, 2);
    const action = State.clipboard.isCut ? 'Moved' : 'Linked';
    setStatusMessage(`${action}: "${State.clipboard.title}" after "${findNodeById(State.view, State.selectedId).title}"`);

    if (State.clipboard.isCut) {
        State.clipboard = null;
    }
}

function copyNoteReference() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }

    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    // Copy reference in smsn format to system clipboard
    const reference = `*:${node.id}:`;
    navigator.clipboard.writeText(reference).then(() => {
        setStatusMessage(`Copied reference: ${reference}`);
    }).catch(() => {
        setStatusMessage('Failed to copy to clipboard');
    });
}

function copyNoteTitle() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }

    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    navigator.clipboard.writeText(node.title).then(() => {
        setStatusMessage(`Copied title: "${node.title}"`);
    }).catch(() => {
        setStatusMessage('Failed to copy to clipboard');
    });
}

function moveNoteUp() {
    if (!State.selectedId || State.selectedId === State.rootId) {
        setStatusMessage('Cannot move this note');
        return;
    }

    const parentId = findParentId(State.view, State.selectedId);
    if (!parentId) return;

    const parent = findNodeById(State.view, parentId);
    if (!parent || !parent.children || parent.children.length < 2) return;

    const index = parent.children.findIndex(c => c.id === State.selectedId);
    if (index <= 0) {
        setStatusMessage('Already at top');
        return;
    }

    reorderChild(parentId, index, index - 1);
}

function moveNoteDown() {
    if (!State.selectedId || State.selectedId === State.rootId) {
        setStatusMessage('Cannot move this note');
        return;
    }

    const parentId = findParentId(State.view, State.selectedId);
    if (!parentId) return;

    const parent = findNodeById(State.view, parentId);
    if (!parent || !parent.children || parent.children.length < 2) return;

    const index = parent.children.findIndex(c => c.id === State.selectedId);
    if (index >= parent.children.length - 1) {
        setStatusMessage('Already at bottom');
        return;
    }

    reorderChild(parentId, index, index + 1);
}

function reorderChild(parentId, fromIndex, toIndex) {
    const selectedId = State.selectedId;

    sendAction({
        action: 'net.fortytwo.smsn.server.actions.ReorderChildren',
        parentId: parentId,
        fromIndex: fromIndex,
        toIndex: toIndex,
        filter: getFilterForServer()
    }, (data) => {
        if (data.success) {
            // Refresh the view and preserve selection
            refreshViewPreserveSelection(selectedId);
            setStatusMessage(fromIndex < toIndex ? 'Moved down' : 'Moved up');
        } else {
            setStatusMessage('Failed to reorder');
        }
    });
}

// =============================================================================
// Tree Utilities
// =============================================================================

function findNodeById(node, id) {
    if (!node) return null;
    if (node.id === id) return node;
    if (node.children) {
        for (const child of node.children) {
            const found = findNodeById(child, id);
            if (found) return found;
        }
    }
    return null;
}

function findParentId(node, childId, parentId = null) {
    if (!node) return null;
    if (node.id === childId) return parentId;
    if (node.children) {
        for (const child of node.children) {
            const found = findParentId(child, childId, node.id);
            if (found) return found;
        }
    }
    return null;
}

function getVisibleNodeIds() {
    const ids = [];
    collectVisibleIds(State.view, true, ids);
    return ids;
}

function collectVisibleIds(node, isRoot, ids) {
    if (!node) return;
    ids.push(node.id);
    const isExpanded = State.expandedNodes.has(node.id);
    if (isExpanded && node.children) {
        for (const child of node.children) {
            collectVisibleIds(child, false, ids);
        }
    }
}

function moveSelection(direction) {
    const visibleIds = getVisibleNodeIds();
    if (visibleIds.length === 0) return;

    const currentIndex = visibleIds.indexOf(State.selectedId);
    let newIndex;

    if (currentIndex === -1) {
        newIndex = direction > 0 ? 0 : visibleIds.length - 1;
    } else {
        newIndex = currentIndex + direction;
        if (newIndex < 0) newIndex = 0;
        if (newIndex >= visibleIds.length) newIndex = visibleIds.length - 1;
    }

    selectNode(visibleIds[newIndex]);

    // Scroll to the selected element in the active pane
    const container = document.getElementById(`tree-container-${State.activePane}`);
    const selectedEl = container.querySelector(`.tree-node[data-id="${State.selectedId}"]`);
    if (selectedEl) {
        selectedEl.scrollIntoView({ block: 'nearest' });
    }
}

// =============================================================================
// Rendering
// =============================================================================

function render() {
    renderPane(0);
    if (State.splitView) {
        renderPane(1);
    }
    updateNodeCount();
}

function renderPane(paneIndex) {
    const containerId = `tree-container-${paneIndex}`;
    const container = document.getElementById(containerId);
    const pane = State.panes[paneIndex];

    if (!pane.view) {
        container.innerHTML = '<div class="loading">No data</div>';
        return;
    }

    // Check if view root is filtered out
    if (!sourcePassesFilter(pane.view.source)) {
        showFilteredRootMessage(paneIndex);
        return;
    }

    // Render using pane-specific state with line counter
    const lineCounter = { value: 1 };
    container.innerHTML = renderNodeForPane(pane.view, true, 0, paneIndex, lineCounter);
}

function renderNodeForPane(node, isRoot, depth, paneIndex, lineCounter) {
    if (!node) return '';

    const pane = State.panes[paneIndex];
    const hasChildren = node.children && node.children.length > 0;
    const isExpanded = pane.expandedNodes.has(node.id);
    const isSelected = pane.selectedId === node.id;
    const isEditing = State.editingNodeId === node.id;

    // Get current line number and increment for next node
    const lineNumber = lineCounter.value++;

    // Determine toggle icon:
    // - expanded with children visible
    // - collapsed but has children loaded (can expand)
    // - has children (numberOfChildren > 0) but not loaded (at view depth edge)
    // - no children at all
    const logicalChildCount = node.numberOfChildren || (node.children ? node.children.length : 0);
    let toggleIcon;
    if (hasChildren) {
        toggleIcon = isExpanded ? '\u25BC' : '\u25B6';
    } else if (logicalChildCount > 0) {
        toggleIcon = '\u25B7';  // Hollow triangle - has children but not loaded
    } else {
        toggleIcon = '\u00B7';  // No children
    }

    const weight = parseFloat(node.weight) || 0.5;
    const source = node.source || 'private';
    const shortcutOrAlias = node.shortcut || node.alias;  // Check both fields
    const hasAlias = !!shortcutOrAlias;
    const hasPriority = node.priority && node.priority > 0;

    const textColor = getNoteColor(source, weight);

    // Line number (shown in left margin, aligned to far left regardless of depth)
    // Each depth level adds 24px of margin, so we offset left to compensate
    const lineNumLeft = 4 - (depth * 24);
    const lineNumHtml = State.showLineNumbers
        ? `<span class="line-number" style="left: ${lineNumLeft}px">${lineNumber}</span>`
        : '';

    const priorityHtml = hasPriority
        ? `<span class="node-priority" title="Priority: ${node.priority.toFixed(1)}">!</span>`
        : `<span class="priority-spacer"></span>`;

    const childCountHtml = hasChildren && !isExpanded
        ? `<span class="node-meta">(${node.numberOfChildren || node.children.length})</span>`
        : '';

    let titleHtml;
    if (isEditing) {
        titleHtml = `<textarea class="edit-input" id="edit-title-input"
            onkeydown="handleEditKeydown(event)"
            onblur="cancelEditTitle()"
            oninput="autoResizeTextarea(this)">${escapeHtml(node.title || '')}</textarea>`;
    } else {
        const titleText = escapeHtml(node.title || '[no title]');
        const aliasClass = hasAlias ? ' has-alias' : '';
        titleHtml = `<span style="color: ${textColor}" class="${aliasClass}" title="${hasAlias ? 'Alias: ' + shortcutOrAlias : ''}">${titleText}</span>`;
    }

    let html = `
        <div class="tree-node ${isRoot ? 'root' : ''} ${isSelected ? 'selected' : ''} ${isEditing ? 'editing' : ''}"
             data-id="${node.id}" data-depth="${depth}" data-pane="${paneIndex}" data-line="${lineNumber}">
            ${lineNumHtml}
            ${priorityHtml}
            <span class="toggle" style="color: ${textColor}" onclick="event.stopPropagation(); toggleExpandPane('${node.id}', ${paneIndex})">${toggleIcon}</span>
            <div class="node-content" onclick="handleNodeClickPane('${node.id}', ${paneIndex})">
                <span class="node-title">${titleHtml}</span>
                ${childCountHtml}
            </div>
        </div>
    `;

    if (hasChildren) {
        html += `<div class="children ${isExpanded ? '' : 'collapsed'}">`;
        for (const child of node.children) {
            html += renderNodeForPane(child, false, depth + 1, paneIndex, lineCounter);
        }
        html += '</div>';
    }

    return html;
}

function renderNode(node, isRoot = false, depth = 0) {
    if (!node) return '';

    const hasChildren = node.children && node.children.length > 0;
    const isExpanded = State.expandedNodes.has(node.id);
    const isSelected = State.selectedId === node.id;
    const isEditing = State.editingNodeId === node.id;

    // Determine toggle icon:
    // - expanded with children visible
    // - collapsed but has children loaded (can expand)
    // - has children (numberOfChildren > 0) but not loaded (at view depth edge)
    // - no children at all
    const logicalChildCount = node.numberOfChildren || (node.children ? node.children.length : 0);
    let toggleIcon;
    if (hasChildren) {
        toggleIcon = isExpanded ? '\u25BC' : '\u25B6';
    } else if (logicalChildCount > 0) {
        toggleIcon = '\u25B7';  // Hollow triangle - has children but not loaded
    } else {
        toggleIcon = '\u00B7';  // No children
    }

    const weight = parseFloat(node.weight) || 0.5;
    const source = node.source || 'private';
    const shortcutOrAlias = node.shortcut || node.alias;  // Check both fields
    const hasAlias = !!shortcutOrAlias;
    const hasPriority = node.priority && node.priority > 0;

    const textColor = getNoteColor(source, weight);

    const priorityHtml = hasPriority
        ? `<span class="node-priority" title="Priority: ${node.priority.toFixed(1)}">!</span>`
        : '';

    const childCountHtml = hasChildren && !isExpanded
        ? `<span class="node-meta">(${node.numberOfChildren || node.children.length})</span>`
        : '';

    let titleHtml;
    if (isEditing) {
        titleHtml = `<textarea class="edit-input" id="edit-title-input"
            onkeydown="handleEditKeydown(event)"
            onblur="cancelEditTitle()"
            oninput="autoResizeTextarea(this)">${escapeHtml(node.title || '')}</textarea>`;
    } else {
        const titleText = escapeHtml(node.title || '[no title]');
        const aliasClass = hasAlias ? ' has-alias' : '';
        titleHtml = `<span style="color: ${textColor}" class="${aliasClass}">${titleText}</span>`;
    }

    let html = `
        <div class="tree-node ${isRoot ? 'root' : ''} ${isSelected ? 'selected' : ''} ${isEditing ? 'editing' : ''}"
             data-id="${node.id}" data-depth="${depth}">
            <span class="toggle" style="color: ${textColor}" onclick="event.stopPropagation(); toggleExpand('${node.id}')">${toggleIcon}</span>
            <div class="node-content" onclick="handleNodeClick('${node.id}')">
                <span class="node-title">${priorityHtml}${titleHtml}</span>
                ${childCountHtml}
            </div>
        </div>
    `;

    if (hasChildren) {
        html += `<div class="children ${isExpanded ? '' : 'collapsed'}">`;
        for (const child of node.children) {
            html += renderNode(child, false, depth + 1);
        }
        html += '</div>';
    }

    return html;
}

function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

function escapeAttr(text) {
    return text.replace(/"/g, '&quot;').replace(/'/g, '&#39;');
}

// =============================================================================
// UI Updates
// =============================================================================

function updateConnectionStatus(status) {
    const el = document.getElementById('connection-status');
    el.className = status;

    switch (status) {
        case 'connected':
            el.textContent = 'Connected';
            break;
        case 'connecting':
            el.textContent = 'Connecting...';
            break;
        case 'disconnected':
            el.textContent = 'Disconnected (reconnecting...)';
            break;
        case 'error':
            el.textContent = 'Connection error';
            break;
    }
}

function setStatusMessage(msg) {
    document.getElementById('status-message').textContent = msg;
}

function updateNodeCount() {
    const count = countNodes(State.view);
    document.getElementById('node-count').textContent = `${count} notes`;
}

function countNodes(node) {
    if (!node) return 0;
    let count = 1;
    if (node.children) {
        for (const child of node.children) {
            count += countNodes(child);
        }
    }
    return count;
}

function updateSourceOptions() {
    const dropdown = document.getElementById('prop-source-dropdown');
    dropdown.innerHTML = '';

    if (State.config && State.config.sources) {
        // Show only sources that pass the current filter
        State.config.sources.forEach(s => {
            if (!sourcePassesFilter(s.name)) return;
            const option = document.createElement('div');
            option.className = 'source-option';
            option.dataset.value = s.name;
            option.innerHTML = `
                <span class="source-option-color" style="background: ${colorToHex(parseColor(s.color))}"></span>
                <span>${s.name}</span>
            `;
            option.addEventListener('click', () => selectSource(s.name));
            dropdown.appendChild(option);
        });
    }

    // Also populate new-note-source dropdown (standard select)
    const newNoteSelect = document.getElementById('new-note-source');
    newNoteSelect.innerHTML = '';  // Clear existing options
    if (State.config && State.config.sources) {
        // Show only sources that pass the current filter
        State.config.sources.forEach(s => {
            if (!sourcePassesFilter(s.name)) return;
            const option = document.createElement('option');
            option.value = s.name;
            option.textContent = s.name;
            newNoteSelect.appendChild(option);
        });
    }
}

function selectSource(sourceName) {
    const sourceConfig = State.sourcesByName[sourceName];
    const select = document.getElementById('prop-source');
    const dropdown = document.getElementById('prop-source-dropdown');

    // Update displayed value
    select.querySelector('.source-select-text').textContent = sourceName;
    select.querySelector('.source-select-color').style.background =
        sourceConfig ? colorToHex(parseColor(sourceConfig.color)) : '#333';
    select.dataset.value = sourceName;

    // Update selected state in dropdown
    dropdown.querySelectorAll('.source-option').forEach(opt => {
        opt.classList.toggle('selected', opt.dataset.value === sourceName);
    });

    // Close dropdown
    dropdown.classList.remove('open');
}

function toggleSourceDropdown() {
    const dropdown = document.getElementById('prop-source-dropdown');
    dropdown.classList.toggle('open');
}

function getSelectedSource() {
    return document.getElementById('prop-source').dataset.value || 'private';
}

function updateSourceLegend() {
    const legend = document.getElementById('source-legend');
    legend.innerHTML = '';

    if (State.config && State.config.sources) {
        // Only show sources that are NOT excluded (visible sources only)
        State.config.sources.forEach(s => {
            if (!State.filter.excludedSources.has(s.name)) {
                const item = document.createElement('div');
                item.className = 'legend-item';
                item.title = s.name;
                item.innerHTML = `
                    <div class="legend-color" style="background: ${colorToHex(parseColor(s.color))}"></div>
                    <span>${s.name}</span>
                `;
                legend.appendChild(item);
            }
        });
    }
}

function updateSourceFilterDropdown() {
    const menu = document.getElementById('source-filter-menu');
    const label = document.getElementById('source-filter-label');
    if (!menu) return;

    menu.innerHTML = '';

    if (State.config && State.config.sources) {
        // Add checkbox for each source
        State.config.sources.forEach(s => {
            const isVisible = !State.filter.excludedSources.has(s.name);
            const item = document.createElement('div');
            item.className = 'source-filter-item';

            const checkbox = document.createElement('input');
            checkbox.type = 'checkbox';
            checkbox.id = `source-filter-${s.name}`;
            checkbox.checked = isVisible;
            checkbox.onchange = (e) => {
                e.stopPropagation();
                toggleSourceFilter(s.name);
            };

            const labelEl = document.createElement('label');
            labelEl.htmlFor = `source-filter-${s.name}`;
            labelEl.innerHTML = `
                <div class="source-filter-color" style="background: ${colorToHex(parseColor(s.color))}"></div>
                <span>${s.name}</span>
            `;

            item.appendChild(checkbox);
            item.appendChild(labelEl);
            item.onclick = (e) => {
                if (e.target !== checkbox) {
                    checkbox.checked = !checkbox.checked;
                    toggleSourceFilter(s.name);
                }
            };
            menu.appendChild(item);
        });

        // Add divider and "Show all" action
        const divider = document.createElement('div');
        divider.className = 'source-filter-divider';
        menu.appendChild(divider);

        const showAll = document.createElement('div');
        showAll.className = 'source-filter-action';
        showAll.textContent = 'Show all';
        showAll.onclick = () => {
            State.filter.excludedSources.clear();
            applySourceFilterChange();
        };
        menu.appendChild(showAll);
    }

    // Update button label to show count if sources are hidden
    if (label) {
        const hiddenCount = State.filter.excludedSources.size;
        const totalCount = State.config?.sources?.length || 0;
        if (hiddenCount === 0) {
            label.textContent = 'Sources';
        } else {
            label.textContent = `Sources (${totalCount - hiddenCount}/${totalCount})`;
        }
    }
}

function toggleSourceFilterDropdown() {
    const menu = document.getElementById('source-filter-menu');
    if (menu) {
        menu.classList.toggle('open');
    }
}

// Close dropdown when clicking outside
document.addEventListener('click', (e) => {
    const dropdown = document.getElementById('source-filter-dropdown');
    const menu = document.getElementById('source-filter-menu');
    if (dropdown && menu && !dropdown.contains(e.target)) {
        menu.classList.remove('open');
    }
});

function toggleSourceFilter(sourceName) {
    if (State.filter.excludedSources.has(sourceName)) {
        State.filter.excludedSources.delete(sourceName);
    } else {
        State.filter.excludedSources.add(sourceName);
    }
    applySourceFilterChange();
}

function applySourceFilterChange() {
    // Update legend and source dropdowns to reflect filter
    updateSourceLegend();
    updateSourceFilterDropdown();
    updateSourceOptions();

    // Refresh current views to apply new filter
    const pane0 = State.panes[0];
    const pane1 = State.panes[1];

    // Check if pane0's root is filtered out
    if (pane0.rootId && !pane0.rootId.startsWith(SEARCH_RESULT_PREFIX)) {
        if (pane0.view && !sourcePassesFilter(pane0.view.source)) {
            showFilteredRootMessage(0);
        } else {
            getViewForPane(pane0.rootId, 0);
        }
    } else if (pane0.lastSearchQuery) {
        reExecuteSearch(pane0.lastSearchQuery, 0);
    }

    // Check if pane1's root is filtered out
    if (State.splitView) {
        if (pane1.rootId && !pane1.rootId.startsWith(SEARCH_RESULT_PREFIX)) {
            if (pane1.view && !sourcePassesFilter(pane1.view.source)) {
                showFilteredRootMessage(1);
            } else {
                getViewForPane(pane1.rootId, 1);
            }
        } else if (pane1.lastSearchQuery) {
            reExecuteSearch(pane1.lastSearchQuery, 1);
        }
    }

    const hiddenCount = State.filter.excludedSources.size;
    if (hiddenCount === 0) {
        setStatusMessage('Showing all sources');
    } else {
        setStatusMessage(`Hiding ${hiddenCount} source${hiddenCount > 1 ? 's' : ''}`);
    }
}

function showFilteredRootMessage(paneIndex) {
    const container = document.getElementById(`tree-container-${paneIndex}`);
    container.innerHTML = `
        <div class="tree-node root">
            <span class="priority-spacer"></span>
            <span class="toggle" style="color: var(--muted-color)">\u00B7</span>
            <div class="node-content">
                <span class="node-title" style="color: var(--muted-color); font-style: italic;">View root not visible</span>
            </div>
        </div>`;
}

function toggleHelp() {
    document.getElementById('help-overlay').classList.toggle('visible');
}

function hideHelp() {
    document.getElementById('help-overlay').classList.remove('visible');
}

// Track which pane the history overlay is showing for
let historyPaneIndex = 0;

function showHistory(paneIndex) {
    historyPaneIndex = paneIndex;
    const pane = State.panes[paneIndex];
    const listEl = document.getElementById('history-list');

    // Include current view as most recent entry
    const currentEntry = createHistoryEntry(pane);
    const allEntries = [...pane.history];
    if (currentEntry) {
        allEntries.push(currentEntry);
    }

    // Filter entries based on excluded sources (search results always pass)
    const filteredEntries = allEntries.filter(entry => {
        if (typeof entry === 'string') return true;  // Old format, can't filter
        if (entry.isSearch) return true;  // Search results always shown
        return sourcePassesFilter(entry.source);
    });

    if (filteredEntries.length === 0) {
        listEl.innerHTML = '<div class="history-empty">No history yet</div>';
    } else {
        listEl.innerHTML = '';
        // Show history in reverse chronological order (most recent first)
        for (let i = filteredEntries.length - 1; i >= 0; i--) {
            const entry = filteredEntries[i];
            const isCurrent = (entry === currentEntry);
            const item = createHistoryListItem(entry, i, isCurrent);
            listEl.appendChild(item);
        }
    }

    document.getElementById('history-overlay').classList.add('visible');
}

function createHistoryListItem(entry, index, isCurrent = false) {
    const item = document.createElement('div');
    item.className = 'history-item' + (isCurrent ? ' history-item-current' : '');

    // Handle both old format (string ID) and new format (object)
    if (typeof entry === 'string') {
        // Old format - just ID
        const isSearch = entry.startsWith(SEARCH_RESULT_PREFIX);
        const title = isSearch ? entry.substring(SEARCH_RESULT_PREFIX.length) : entry;
        item.innerHTML = `
            <span class="history-item-icon">${isSearch ? '&#128269;' : '&#9654;'}</span>
            <span class="history-item-title ${isSearch ? 'history-item-search' : ''}">${escapeHtml(title)}</span>
            ${isCurrent ? '<span class="history-item-meta">(current)</span>' : ''}
        `;
        if (!isCurrent) {
            item.onclick = () => {
                hideHistory();
                navigateFromHistory(entry, historyPaneIndex, index);
            };
        }
    } else {
        // New format - object with metadata
        const isSearch = entry.isSearch;
        const icon = isSearch ? '&#128269;' : (entry.viewStyle === 'backward' ? '&#9650;' : '&#9660;');
        const timeStr = isCurrent ? '(current)' : formatHistoryTime(entry.timestamp);
        item.innerHTML = `
            <span class="history-item-icon">${icon}</span>
            <span class="history-item-title ${isSearch ? 'history-item-search' : ''}">${escapeHtml(entry.title || entry.searchQuery || entry.id)}</span>
            <span class="history-item-meta">${timeStr}</span>
        `;
        if (!isCurrent) {
            item.onclick = () => {
                hideHistory();
                navigateFromHistory(entry, historyPaneIndex, index);
            };
        }
    }

    return item;
}

function formatHistoryTime(timestamp) {
    if (!timestamp) return '';
    const date = new Date(timestamp);
    const now = new Date();
    const diffMs = now - date;
    const diffMins = Math.floor(diffMs / 60000);

    if (diffMins < 1) return 'now';
    if (diffMins < 60) return `${diffMins}m ago`;
    const diffHours = Math.floor(diffMins / 60);
    if (diffHours < 24) return `${diffHours}h ago`;
    return date.toLocaleDateString();
}

function navigateFromHistory(entry, paneIndex, historyIndex) {
    const pane = State.panes[paneIndex];

    // Remove entries from history after this point and add them to forwardHistory
    const removed = pane.history.splice(historyIndex + 1);
    pane.forwardHistory = [...removed.reverse(), ...pane.forwardHistory];

    // Navigate to the entry (don't add it to history since it's already there)
    navigateToEntryNoHistory(entry, paneIndex);
}

function navigateToEntryNoHistory(entry, paneIndex) {
    // Navigate to a history entry without adding to history
    if (typeof entry === 'string') {
        // Old format - just ID
        if (entry.startsWith(SEARCH_RESULT_PREFIX)) {
            const query = entry.substring(SEARCH_RESULT_PREFIX.length);
            reExecuteSearchNoHistory(query, paneIndex);
        } else {
            navigateToIdNoHistory(entry, paneIndex);
        }
    } else {
        // New format - object
        if (entry.isSearch) {
            reExecuteSearchNoHistory(entry.searchQuery, paneIndex);
        } else {
            navigateToIdNoHistory(entry.id, paneIndex, entry.viewStyle, entry.viewDepth);
        }
    }
}

function navigateToIdNoHistory(id, paneIndex, viewStyle, viewDepth) {
    const pane = State.panes[paneIndex];
    if (viewStyle) pane.viewStyle = viewStyle;
    if (viewDepth) pane.viewDepth = viewDepth;

    // Update depth selector UI
    const depthSelect = document.getElementById(`depth-select-${paneIndex}`);
    if (depthSelect) depthSelect.value = pane.viewDepth;

    // Fetch and display the view (uses getViewForPane which doesn't add to history)
    getViewForPane(id, paneIndex);
    updateToolbar();
}

function reExecuteSearchNoHistory(query, paneIndex) {
    const pane = State.panes[paneIndex];
    pane.lastSearchQuery = query;

    sendAction({
        action: 'net.fortytwo.smsn.server.actions.Search',
        queryType: 'FullText',
        query: query,
        height: pane.viewDepth,
        valueCutoff: pane.filter?.minWeight || 0.0
    }, (data) => {
        if (data.view) {
            const view = data.view;
            pane.rootId = SEARCH_RESULT_PREFIX + query;
            pane.view = view;
            pane.selectedId = null;
            pane.expandedNodes = new Set();

            // Don't add to history

            if (paneIndex === State.activePane) {
                State.rootId = pane.rootId;
                State.view = pane.view;
                State.selectedId = pane.selectedId;
                State.expandedNodes = pane.expandedNodes;
            }

            renderPane(paneIndex);
            updateNodeCount();
        }
    });
}

function hideHistory() {
    document.getElementById('history-overlay').classList.remove('visible');
}

// =============================================================================
// Event Handlers
// =============================================================================

function handleNodeClick(nodeId) {
    if (State.editingNodeId) return;

    if (State.selectedId === nodeId) {
        visitTarget(nodeId);
    } else {
        selectNode(nodeId);
    }
}

function handleEditKeydown(event) {
    if (event.key === 'Enter' && !event.shiftKey) {
        // Enter (without Shift) saves the title
        event.preventDefault();
        saveEditTitle();
    } else if (event.key === 'Enter' && event.shiftKey) {
        // Shift+Enter inserts a newline - let it happen, then resize
        setTimeout(() => {
            autoResizeTextarea(event.target);
        }, 0);
    } else if (event.key === 'Escape') {
        event.preventDefault();
        cancelEditTitle();
    }
}

// =============================================================================
// Keyboard Handling - Emacs-style Chords
// =============================================================================

// Emacs-style keybindings: C-c and C-x prefixes followed by second key
// Maps 'C-c X' and 'C-x X' to actions, matching smsn-mode/Emacs where possible
// C-c C-t is a sub-prefix for "set" operations (C-c C-t C-w for weight, etc.)
const chordBindings = {
    'C-c': {
    // C-c t - visit target (push view into selected note)
    't': () => { if (State.selectedId) visitTarget(State.selectedId); },
    // C-c b - backward view
    'b': () => setViewStyle('backward'),
    // C-c f - forward view / find roots
    'f': () => {
        if (State.rootId) {
            setViewStyle('forward');
        } else {
            findRoots();
        }
    },
    // C-c s - search (title query)
    's': () => {
        const input = document.getElementById(`search-input-${State.activePane}`);
        if (input) {
            input.focus();
            input.select();  // Select existing text so typing replaces it
        }
    },
    // C-c u - update/refresh view
    'u': () => refreshView(),
    // C-c n - new note
    'n': () => showNewNote('child'),
    // C-c p - push view to server (DISABLED - buggy, mangles data)
    // 'p': () => pushViewToServer(),
    // C-c h - history (back)
    'h': () => popView(),
    // C-c H - view full history (uppercase H)
    'H': () => showHistory(State.activePane),
    // C-c x - shortcut query (go to alias)
    'x': () => gotoAlias(),
    // C-c d - duplicates / delete
    'd': () => showConfirmDelete(),
    // C-c r - copy reference to clipboard (smsn-mode)
    'r': () => copyNoteReference(),
    // C-c v - copy title to clipboard (smsn-mode)
    'v': () => copyNoteTitle(),
    // C-c C-d - set view depth (prompts for number)
    'C-d': () => promptViewDepth(),
    // C-c C-f - find roots
    'C-f': () => findRoots(),
    // C-c C-t - sub-prefix for "set" operations (enters C-c C-t mode)
    'C-t': 'C-c C-t',  // Special marker to enter sub-chord mode
    // C-c C-v - sub-prefix for view options
    'C-v': 'C-c C-v',
    },
    // C-c C-t sub-bindings for setting properties
    'C-c C-t': {
        // C-c C-t i - show property info overlay
        'i': () => showProperties(),
        // C-c C-t C-w - set weight (followed by 0-4)
        'C-w': () => promptSetWeightNumeric(),
        // C-c C-t C-s - set source
        'C-s': () => promptSetSource(),
        // C-c C-t C-p - set priority (followed by 0-4)
        'C-p': () => promptSetPriorityNumeric(),
        // C-c C-t C-a - set shortcut/alias
        'C-a': () => promptSetShortcut(),
    },
    // C-c C-v sub-bindings for view options
    'C-c C-v': {
        // C-c C-v o - toggle line wrap (truncate-lines in Emacs)
        'o': () => toggleWrapTitles(),
        // C-c C-v l - toggle line numbers
        'l': () => toggleLineNumbers(),
    },
    'C-x': {
        // C-x k - kill buffer (go back/pop view)
        'k': () => popView(),
        // C-x 3 - split window (toggle split view)
        '3': () => toggleSplit(),
        // C-x 1 - delete other windows (close split)
        '1': () => { if (State.splitView) toggleSplit(); },
        // C-x o - other window (switch pane)
        'o': () => { if (State.splitView) setActivePane(State.activePane === 0 ? 1 : 0); },
    }
};

function showChordStatus(text) {
    setStatusMessage(text);
}

function clearChordStatus() {
    setStatusMessage('');
}

function promptViewDepth() {
    const depth = prompt('View depth (1-4):', getPane().viewDepth);
    if (depth) {
        const d = parseInt(depth, 10);
        if (d >= 1 && d <= 4) {
            setViewDepth(State.activePane, d);
        }
    }
}

function promptSetWeight() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }
    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    // smsn-mode uses 0-4 scale mapped to 0.0-1.0 (n/4)
    const currentWeight = node.weight || 0.5;
    const input = prompt(
        'Weight (0-4, or 0.0-1.0):\n' +
        '  0=0.00  1=0.25  2=0.50  3=0.75  4=1.00',
        currentWeight
    );
    if (input === null) return;

    let weight = parseFloat(input);
    if (isNaN(weight)) return;

    // If integer 0-4, convert to 0.0-1.0 scale
    if (Number.isInteger(weight) && weight >= 0 && weight <= 4) {
        weight = weight / 4.0;
    }

    // Clamp to valid range
    weight = Math.max(0.0, Math.min(1.0, weight));

    setProperty(State.selectedId, 'weight', weight, () => {
        refreshView();
        setStatusMessage(`Weight set to ${weight.toFixed(2)}`);
    });
}

function promptSetSource() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }
    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    // Build source options from config
    let sourceList = 'Sources:\n';
    let sourceNames = [];
    if (State.config && State.config.sources) {
        State.config.sources.forEach((s, i) => {
            sourceList += `  ${s.code || i}: ${s.name}\n`;
            sourceNames.push(s.name);
        });
    }

    const input = prompt(
        sourceList + '\nEnter source code or name:',
        node.source || 'private'
    );
    if (input === null || input.trim() === '') return;

    // Find matching source by code or name
    let sourceName = null;
    if (State.config && State.config.sources) {
        for (const s of State.config.sources) {
            if (s.code === input || s.name === input || s.name.toLowerCase() === input.toLowerCase()) {
                sourceName = s.name;
                break;
            }
        }
    }

    if (!sourceName) {
        setStatusMessage('Unknown source: ' + input);
        return;
    }

    setProperty(State.selectedId, 'source', sourceName, () => {
        refreshView();
        setStatusMessage(`Source set to ${sourceName}`);
    });
}

function promptSetPriority() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }
    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    const currentPriority = node.priority || 0;
    const input = prompt(
        'Priority (0-4, or 0.0-1.0):\n' +
        '  0=none  1=0.25  2=0.50  3=0.75  4=1.00',
        currentPriority > 0 ? currentPriority : '0'
    );
    if (input === null) return;

    let priority = parseFloat(input);
    if (isNaN(priority)) return;

    // If integer 0-4, convert to 0.0-1.0 scale
    if (Number.isInteger(priority) && priority >= 0 && priority <= 4) {
        priority = priority / 4.0;
    }

    // Clamp to valid range
    priority = Math.max(0.0, Math.min(1.0, priority));

    setProperty(State.selectedId, 'priority', priority, () => {
        refreshView();
        setStatusMessage(priority > 0 ? `Priority set to ${priority.toFixed(2)}` : 'Priority cleared');
    });
}

// Numeric prompt functions for C-c C-t bindings (wait for single digit 0-4)
function promptSetWeightNumeric() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }
    setStatusMessage('Weight (0-4):');
    State.pendingNumericAction = 'weight';
}

function promptSetPriorityNumeric() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }
    setStatusMessage('Priority (0-4):');
    State.pendingNumericAction = 'priority';
}

function promptSetShortcut() {
    if (!State.selectedId) {
        setStatusMessage('No note selected');
        return;
    }
    const node = findNodeById(State.view, State.selectedId);
    if (!node) return;

    const input = prompt('Shortcut (alias):', node.shortcut || '');
    if (input === null) return;

    setProperty(State.selectedId, 'shortcut', input.trim() || null, () => {
        refreshView();
        setStatusMessage(input.trim() ? `Shortcut set to "${input.trim()}"` : 'Shortcut cleared');
    });
}

function handleNumericInput(digit) {
    const node = findNodeById(State.view, State.selectedId);
    if (!node) {
        State.pendingNumericAction = null;
        return;
    }

    if (State.pendingNumericAction === 'weight') {
        const weight = digit / 4.0;
        const oldWeight = node.weight || 0.5;
        setProperty(State.selectedId, 'weight', weight, () => {
            refreshView();
            setStatusMessage(`Weight set to ${weight.toFixed(2)}`);
        }, oldWeight);
    } else if (State.pendingNumericAction === 'priority') {
        const priority = digit / 4.0;
        const oldPriority = node.priority || 0;
        setProperty(State.selectedId, 'priority', priority, () => {
            refreshView();
            setStatusMessage(priority > 0 ? `Priority set to ${priority.toFixed(2)}` : 'Priority cleared');
        }, oldPriority);
    }
    State.pendingNumericAction = null;
}

// =============================================================================
// Main Keyboard Event Handler
// =============================================================================

function setupKeyboardHandler() {
    document.addEventListener('keydown', (e) => {
        const activeSearchInput = document.getElementById(`search-input-${State.activePane}`);
        const inSearch = document.activeElement && document.activeElement.classList.contains('search-input');
        const helpVisible = document.getElementById('help-overlay').classList.contains('visible');
        const propsVisible = document.getElementById('properties-overlay').classList.contains('visible');
        const confirmVisible = document.getElementById('confirm-overlay').classList.contains('visible');
        const newNoteVisible = document.getElementById('new-note-overlay').classList.contains('visible');
        const historyVisible = document.getElementById('history-overlay').classList.contains('visible');
        const inOverlay = propsVisible || confirmVisible || newNoteVisible || historyVisible;

        // Handle Escape - always cancels pending chord, numeric action, or current action
        if (e.key === 'Escape') {
            if (State.pendingChord) {
                State.pendingChord = null;
                clearChordStatus();
                return;
            }
            if (State.pendingNumericAction) {
                State.pendingNumericAction = null;
                setStatusMessage('Cancelled');
                return;
            }
            if (State.editingNodeId) {
                cancelEditTitle();
            } else if (helpVisible) {
                hideHelp();
            } else if (propsVisible) {
                hideProperties();
            } else if (confirmVisible) {
                hideConfirm();
                return;
            } else if (newNoteVisible) {
                hideNewNote();
            } else if (historyVisible) {
                hideHistory();
            } else if (inSearch) {
                document.activeElement.value = '';
                document.activeElement.blur();
            }
            return;
        }

        // Handle confirm dialog keyboard navigation
        if (confirmVisible) {
            const removeBtn = document.getElementById('confirm-remove-btn');
            const cancelBtn = document.getElementById('confirm-cancel-btn');
            if (e.key === 'Enter') {
                e.preventDefault();
                // Execute whichever button is focused, or Remove by default
                if (document.activeElement === cancelBtn) {
                    hideConfirm();
                } else {
                    confirmDelete();
                }
                return;
            } else if (e.key === 'ArrowLeft' || e.key === 'ArrowRight' || e.key === 'Tab') {
                e.preventDefault();
                // Toggle focus between buttons
                if (document.activeElement === removeBtn) {
                    cancelBtn.focus();
                } else {
                    removeBtn.focus();
                }
                return;
            }
            return;  // Don't process other keys when confirm dialog is open
        }

        // Handle numeric input for weight/priority (after C-c C-t C-w or C-c C-t C-p)
        if (State.pendingNumericAction) {
            const digit = parseInt(e.key);
            if (!isNaN(digit) && digit >= 0 && digit <= 4) {
                e.preventDefault();
                handleNumericInput(digit);
                return;
            } else if (e.key !== 'Shift' && e.key !== 'Control' && e.key !== 'Alt' && e.key !== 'Meta') {
                // Invalid key, cancel
                State.pendingNumericAction = null;
                setStatusMessage('Invalid input, expected 0-4');
                return;
            }
        }

        // Handle C-c and C-x chord initiation
        if (e.ctrlKey && (e.key === 'c' || e.key === 'x')) {
            if (!State.editingNodeId && !inOverlay && !inSearch) {
                e.preventDefault();
                State.pendingChord = 'C-' + e.key;
                showChordStatus(State.pendingChord + '-');
                return;
            }
        }

        // Emacs standard bindings (single keys with modifiers)
        // Only process these if NOT in a pending chord sequence
        if (!State.editingNodeId && !inOverlay && !inSearch && !State.pendingChord) {
            // C-w - kill/cut region (cut note)
            if (e.ctrlKey && e.key === 'w') {
                e.preventDefault();
                cutNote();
                return;
            }
            // M-w - copy region (copy note) - Alt+w
            if (e.altKey && e.key === 'w') {
                e.preventDefault();
                copyNote();
                return;
            }
            // C-y - yank/paste as child
            if (e.ctrlKey && !e.shiftKey && e.key === 'y') {
                e.preventDefault();
                pasteNote();
                return;
            }
            // C-S-y - yank/paste as sibling
            if (e.ctrlKey && e.shiftKey && e.key === 'Y') {
                e.preventDefault();
                pasteNoteAsSibling();
                return;
            }
            // C-/ or C-_ - undo (placeholder for now)
            if (e.ctrlKey && (e.key === '/' || e.key === '_')) {
                e.preventDefault();
                undo();
                return;
            }
            // M-up / M-down - move note up/down in parent
            if (e.altKey && e.key === 'ArrowUp') {
                e.preventDefault();
                moveNoteUp();
                return;
            }
            if (e.altKey && e.key === 'ArrowDown') {
                e.preventDefault();
                moveNoteDown();
                return;
            }
        }

        // Handle second/third key of chord
        if (State.pendingChord && chordBindings[State.pendingChord]) {
            // Prevent browser default IMMEDIATELY to stop Ctrl+W from closing tab, etc.
            e.preventDefault();
            e.stopPropagation();

            const prefix = State.pendingChord;

            // Build the chord key (e.g., 't' or 'C-d' for Ctrl+d)
            let chordKey = e.key;
            if (e.ctrlKey && e.key !== 'Control') {
                chordKey = 'C-' + e.key;
            }

            const action = chordBindings[prefix][chordKey];
            if (typeof action === 'string') {
                // This is a sub-prefix (e.g., 'C-c C-t'), enter that mode
                State.pendingChord = action;
                showChordStatus(action + '-');
            } else if (typeof action === 'function') {
                State.pendingChord = null;
                clearChordStatus();
                action();
            } else {
                State.pendingChord = null;
                clearChordStatus();
                setStatusMessage(`Unknown chord: ${prefix} ${chordKey}`);
            }
            return;
        }

        if (State.editingNodeId) return;
        if (inOverlay) return;

        if (helpVisible) {
            hideHelp();
            return;
        }

        if (inSearch) {
            // Escape cancels command mode and returns focus to tree
            if (e.key === 'Escape') {
                e.preventDefault();
                document.activeElement.value = '';
                document.activeElement.blur();
                focusTreeContainer();
                return;
            }
            if (e.key === 'Enter') {
                e.preventDefault();
                e.stopPropagation();
                const query = document.activeElement.value.trim();
                // Determine which pane this search input belongs to
                const inputId = document.activeElement.id;
                const paneIndex = inputId.endsWith('-0') ? 0 : 1;
                if (query) {
                    // Check if this is a command (starts with :)
                    if (query.startsWith(':')) {
                        const input = document.activeElement;
                        input.value = '';
                        input.blur();
                        // Check for line number (:42)
                        const lineMatch = query.match(/^:(\d+)$/);
                        if (lineMatch) {
                            const lineNum = parseInt(lineMatch[1], 10);
                            setActivePane(paneIndex);
                            gotoLineNumber(lineNum);
                        } else {
                            // Invalid command
                            setStatusMessage(`Invalid command: ${query}`);
                            focusTreeContainer();
                        }
                    } else {
                        // Regular search
                        searchInPane(query, paneIndex);
                    }
                }
            }
            return;
        }

        // Simple (non-chord) key bindings for quick access
        switch (e.key) {
            case 'Tab':
                if (State.splitView) {
                    e.preventDefault();
                    setActivePane(State.activePane === 0 ? 1 : 0);
                }
                break;
            case '/':
                e.preventDefault();
                if (activeSearchInput) activeSearchInput.focus();
                break;
            case '?':
                toggleHelp();
                break;
            // Vim-style navigation (kept for convenience)
            case 'j':
            case 'ArrowDown':
                e.preventDefault();
                moveSelection(1);
                break;
            case 'k':
            case 'ArrowUp':
                e.preventDefault();
                moveSelection(-1);
                break;
            case 'h':
            case 'ArrowLeft':
                e.preventDefault();
                if (State.selectedId && getPane().expandedNodes.has(State.selectedId)) {
                    toggleExpand(State.selectedId);
                }
                break;
            case 'l':
            case 'ArrowRight':
                e.preventDefault();
                if (State.selectedId) {
                    const node = findNodeById(State.view, State.selectedId);
                    if (node && node.children && node.children.length > 0 && !getPane().expandedNodes.has(State.selectedId)) {
                        toggleExpand(State.selectedId);
                    }
                }
                break;
            case 'Enter':
                if (State.selectedId) {
                    visitTarget(State.selectedId);
                }
                break;
            case 'Backspace':
                e.preventDefault();
                popView();
                break;
            // Quick access keys (in addition to C-c chords)
            case 'r':
                refreshView();
                break;
            case 'e':
                if (State.selectedId) {
                    startEditTitle(State.selectedId);
                }
                break;
            case 'n':
                e.preventDefault();
                showNewNote('child');
                break;
            case 'N':
                e.preventDefault();
                showNewNote('sibling');
                break;
            case 'p':
                showProperties();
                break;
            case 'd':
                showConfirmDelete();
                break;
            case 'g':
                gotoAlias();
                break;
            case ':':
                // Vim-style command mode - focus search and start with colon
                e.preventDefault();
                if (activeSearchInput) {
                    activeSearchInput.value = ':';
                    activeSearchInput.focus();
                    // Place cursor at end
                    activeSearchInput.setSelectionRange(1, 1);
                }
                break;
        }
    });
}

// =============================================================================
// Event Listener Setup (called after DOM ready)
// =============================================================================

function setupEventListeners() {
    // Property overlay sliders
    document.getElementById('prop-weight').addEventListener('input', (e) => {
        document.getElementById('prop-weight-value').textContent = parseFloat(e.target.value).toFixed(1);
    });

    document.getElementById('prop-priority').addEventListener('input', (e) => {
        const val = parseFloat(e.target.value);
        document.getElementById('prop-priority-value').textContent = val > 0 ? val.toFixed(1) : '-';
    });

    // New note overlay sliders
    document.getElementById('new-note-weight').addEventListener('input', (e) => {
        document.getElementById('new-note-weight-value').textContent = parseFloat(e.target.value).toFixed(1);
    });

    document.getElementById('new-note-priority').addEventListener('input', (e) => {
        const val = parseFloat(e.target.value);
        document.getElementById('new-note-priority-value').textContent = val > 0 ? val.toFixed(1) : '-';
    });

    // New note title input - update Create button state on input
    document.getElementById('new-note-input').addEventListener('input', (e) => {
        updateNewNoteCreateButton();
    });

    // New note input - Enter to create, Escape to cancel
    document.getElementById('new-note-input').addEventListener('keydown', (e) => {
        if (e.key === 'Enter') {
            e.preventDefault();
            createNewNote();
        } else if (e.key === 'Escape') {
            hideNewNote();
        }
    });

    // Allow Escape from any field in the new note overlay
    document.getElementById('new-note-overlay').addEventListener('keydown', (e) => {
        if (e.key === 'Escape') {
            hideNewNote();
        }
    });

    // Custom source dropdown
    document.getElementById('prop-source').addEventListener('click', (e) => {
        e.stopPropagation();
        toggleSourceDropdown();
    });

    // Close source dropdown when clicking outside
    document.addEventListener('click', (e) => {
        const dropdown = document.getElementById('prop-source-dropdown');
        const select = document.getElementById('prop-source');
        if (!dropdown.contains(e.target) && !select.contains(e.target)) {
            dropdown.classList.remove('open');
        }
    });

    // Keyboard navigation for source dropdown
    document.getElementById('prop-source').addEventListener('keydown', (e) => {
        const dropdown = document.getElementById('prop-source-dropdown');
        if (e.key === 'Enter' || e.key === ' ') {
            e.preventDefault();
            toggleSourceDropdown();
        } else if (e.key === 'Escape') {
            dropdown.classList.remove('open');
        } else if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
            e.preventDefault();
            if (!dropdown.classList.contains('open')) {
                toggleSourceDropdown();
            } else {
                // Navigate through options
                const options = dropdown.querySelectorAll('.source-option');
                const currentValue = getSelectedSource();
                let currentIndex = Array.from(options).findIndex(o => o.dataset.value === currentValue);
                if (e.key === 'ArrowDown') {
                    currentIndex = Math.min(currentIndex + 1, options.length - 1);
                } else {
                    currentIndex = Math.max(currentIndex - 1, 0);
                }
                selectSource(options[currentIndex].dataset.value);
                dropdown.classList.add('open'); // Keep open while navigating
            }
        }
    });

    // Set up keyboard handler
    setupKeyboardHandler();
}

// =============================================================================
// Initialization
// =============================================================================

function init() {
    setupEventListeners();
    updateToolbar();  // Set initial disabled states
    connect();
    // Focus the search bar on initial load
    const searchInput = document.getElementById('search-input-0');
    if (searchInput) searchInput.focus();
}

// Start when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
} else {
    init();
}
