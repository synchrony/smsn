// =============================================================================
// Tree Utilities
// =============================================================================

export function findNodeById(node, id) {
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

export function findParentId(node, childId, parentId = null) {
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

export function countNodes(node) {
    if (!node) return 0;
    let count = 1;
    if (node.children) {
        for (const child of node.children) {
            count += countNodes(child);
        }
    }
    return count;
}

export function countTotalLines(node) {
    if (!node) return 0;
    let count = 1;  // Count this node
    if (node.children) {
        for (const child of node.children) {
            count += countTotalLines(child);
        }
    }
    return count;
}

export function findPathToLine(node, targetLine, counter, path = []) {
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

// =============================================================================
// HTML Utilities
// =============================================================================

export function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

export function escapeAttr(text) {
    return text.replace(/"/g, '&quot;').replace(/'/g, '&#39;');
}

// =============================================================================
// Status Messages
// =============================================================================

export function setStatusMessage(msg) {
    const el = document.getElementById('status-message');
    if (el) el.textContent = msg;
}
