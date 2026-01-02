# Semantic Synchrony Web UI

A browser-based frontend for Semantic Synchrony, providing a lightweight alternative to smsn-mode for viewing and editing your knowledge graph.

## Quick Start

1. **Start SmSn Server** (see Installation guide)

2. **Open the UI** - Simply open `index.html` in a browser:
   ```bash
   open index.html
   # or
   firefox index.html
   ```

3. **Connect** - The UI automatically connects to `ws://localhost:8182/smsn`

## Features

### Navigation

- **Tree View**: Hierarchical display of notes with expand/collapse
- **Search**: Full-text search (press `/` to focus search box)
- **Push/Pop**: Click to select, double-click or Enter to focus (push view)
- **History**: Backspace to go back to previous view (including search results)
- **Keyboard-Driven**: Vim-style navigation (j/k/h/l)
- **Split View**: Side-by-side panes for comparing or cross-referencing notes
- **View Depth**: Adjustable tree depth (1-4 levels)
- **Go to Alias**: Press `g` to navigate to a shortcut or open URLs

### Editing

- **Edit Title**: Press `e` to edit the selected note's title inline
- **New Child**: Press `n` to create a new child note under the selected node
- **New Sibling**: Press `N` (shift+n) to create a sibling note after the selected node
- **Properties**: Press `p` to edit weight, priority, source, and shortcut
- **Delete**: Press `d` to remove a note from its parent (with confirmation)

### Not Included

- Whole-graph operations (import/export) - use smsn-mode for these
- Admin operations
- Multi-parent linking (notes can only be added as single children via this UI)

## Keyboard Shortcuts

The Web UI supports both quick single-key shortcuts and Emacs-style `C-c` chord sequences (matching smsn-mode where possible).

### Quick Keys

| Key | Action |
|-----|--------|
| `Enter` | Focus selected note (push view) |
| `Backspace` | Go back in history (pop view) |
| `j` / `k` | Move selection down / up |
| `h` / `l` | Collapse / expand node |
| `/` | Focus search input |
| `r` | Refresh current view |
| `g` | Go to alias (shortcut) or open URL |
| `e` | Edit selected note title inline |
| `n` | Create new child note |
| `N` | Create new sibling note |
| `p` | Edit properties |
| `d` | Delete selected note |
| `Tab` | Switch pane (in split view) |
| `?` | Show keyboard shortcuts |
| `Escape` | Cancel / close dialog |

### Emacs-style Chords (C-c ...)

Press `Ctrl+C` followed by the second key. The status bar shows `C-c-` while waiting.

| Chord | Action |
|-------|--------|
| `C-c t` | Push view (focus selected note) |
| `C-c h` | Pop view (history back) |
| `C-c b` | Backward view (show parents) |
| `C-c f` | Forward view (show children) |
| `C-c s` | Search (focus search input) |
| `C-c u` | Update/refresh view |
| `C-c n` | New child note |
| `C-c p` | Edit properties |
| `C-c d` | Delete note |
| `C-c x` | Go to shortcut/alias |
| `C-c r` | Copy note reference to clipboard |
| `C-c v` | Copy note title to clipboard |
| `C-c C-d` | Set view depth (prompts) |
| `C-c C-f` | Find roots |

### Emacs-style Chords (C-x ...)

| Chord | Action |
|-------|--------|
| `C-x k` | Kill buffer (pop view / go back) |
| `C-x 3` | Split window (toggle split view) |
| `C-x 1` | Delete other windows (close split) |
| `C-x o` | Other window (switch pane) |

### Standard Emacs Bindings

| Key | Action |
|-----|--------|
| `C-w` | Kill/cut note |
| `M-w` | Copy note |
| `C-y` | Yank/paste as child |
| `C-S-y` | Yank/paste as sibling |
| `M-↑` / `M-↓` | Move note up/down in parent |

## Properties

Each note has the following editable properties:

| Property | Range | Description |
|----------|-------|-------------|
| **Title** | text | The note's display text |
| **Weight** | 0.1 - 1.0 | Importance/relevance (affects search ranking) |
| **Priority** | 0.0 - 1.0 | TODO priority (0 = no priority) |
| **Source** | private/personal/public/universal | Visibility tier |
| **Shortcut** | text | Quick-access name (must be unique) |

## Configuration

Edit the `State` object at the top of the `<script>` section to customize:

```javascript
const State = {
    serverUrl: 'ws://localhost:8182/smsn',  // Change if server is elsewhere
    filter: {
        minSource: 'private',      // Visibility filter
        defaultSource: 'private',
        minWeight: 0.0,            // Weight filter
        defaultWeight: 0.5
    },
    // ...
};
```

## Comparison with smsn-mode

| Feature | smsn-mode | Web UI |
|---------|-----------|--------|
| View tree | Yes | Yes |
| Search | Yes | Yes |
| Navigate (push/pop) | Yes | Yes |
| Edit title | Yes | Yes |
| Create notes | Yes | Yes |
| Delete notes | Yes | Yes |
| Set properties | Yes | Yes |
| Split view | Yes | Yes |
| View depth control | Yes | Yes |
| Go to alias/URL | Yes | Yes |
| Multi-parent linking | Yes | No |
| Import/Export | Yes | No |
| Keyboard shortcuts | Extensive | Basic |
| Works without Emacs | No | Yes |

## Development

This is a single-file vanilla JavaScript application designed for:

1. **Simplicity**: No build step, no dependencies
2. **Readability**: Clear separation of state, actions, and rendering
3. **Portability**: Works from file:// or any static server
4. **Future Migration**: Structured to enable React migration if needed

### Architecture

```
State (data)
    |
    v
render() - produces DOM from state
    |
    v
Event handlers - update state, call render()
    |
    v
WebSocket - server communication (sendAction)
```

### Key Design Decisions

1. **Single render function**: All UI updates go through `render()`, making the code React-migration-friendly
2. **Callback-based async**: `sendAction(action, callback)` handles responses
3. **Wiki format for updates**: Uses `UpdateView` with wiki format for creating/deleting notes
4. **SetProperties for edits**: Individual property changes use `SetProperties` action

### Adding Features

1. Add state variables to `State` object
2. Add server action function (like `getView`, `search`)
3. Update `render()` to reflect new state
4. Add event handlers/keyboard shortcuts

## Troubleshooting

**"Disconnected (reconnecting...)"**
- Ensure SmSn Server is running on port 8182
- Check browser console for detailed errors

**No notes appearing**
- The UI calls `FindRoots` on connect - ensure your graph has root notes
- Try searching for a known note title

**Edit not saving**
- Check browser console for error messages
- Ensure the title is not empty

**CORS errors**
- If serving from a different origin, Gremlin Server may need CORS configuration
- Running from `file://` should work without CORS issues

## Browser Support

Tested on modern browsers (Chrome, Firefox, Safari). Requires:
- WebSocket API
- ES6+ JavaScript features

## Future Enhancements

Potential improvements for later phases:

- Drag-and-drop reordering
- Multi-select operations
- Undo/redo
- Dark/light theme toggle
- Mobile-optimized layout
- Offline mode with sync
- Multi-parent linking
