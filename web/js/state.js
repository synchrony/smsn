// =============================================================================
// State Management
// =============================================================================

export const State = {
    // Connection
    ws: null,
    connected: false,
    serverUrl: 'ws://localhost:8182/smsn',

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
export const SEARCH_RESULT_PREFIX = '__search__:';

// Get current pane state
export function getPane() {
    return State.panes[State.activePane];
}

// Create a history entry with all information needed to restore a view
export function createHistoryEntry(pane) {
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
export function sourcePassesFilter(source) {
    if (!source) return true;  // Unknown sources pass
    return !State.filter.excludedSources.has(source);
}

// Get the list of included sources for server requests
// Returns empty array if all sources are included (no exclusions)
export function getIncludedSourcesForServer() {
    if (State.filter.excludedSources.size === 0) {
        return [];  // Empty means include all
    }
    // Return all sources except excluded ones
    const allSources = State.config.sources.map(s => s.name);
    return allSources.filter(s => !State.filter.excludedSources.has(s));
}

// Build filter object for server requests
export function getFilterForServer() {
    return {
        includedSources: getIncludedSourcesForServer(),
        defaultSource: State.filter.defaultSource,
        minWeight: State.filter.minWeight,
        defaultWeight: State.filter.defaultWeight
    };
}

// Initialize pane 0 to share references with State
export function initializePaneState() {
    const pane = State.panes[0];
    pane.history = State.history;
    pane.forwardHistory = State.forwardHistory;
    pane.expandedNodes = State.expandedNodes;
}
