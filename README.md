# Semantic Synchrony

A personal knowledge graph that anyone can use.

## Overview

Semantic Synchrony (SmSn) is a tool for building and navigating a personal knowledge graph. Knowledge graphs are the same kind of data structure that Google, Siri, and Alexa use to answer questions about the world. SmSn makes this structure accessible to individuals using a simple, keyboard-driven interface.

Unlike a filesystem or outliner, SmSn organizes notes in a **graph**: any note can have multiple parents, so information can live in many places simultaneously. There is no tension between putting a note "here" or "there" -- it can be in both. This mirrors how people actually think: associatively, not hierarchically.

Each note in the graph is called an **atom**. Atoms have a title, optional body text, and ordered lists of children. They are stored as plain-text `.smsn` files (one per atom), are human-readable, and can be version-controlled with Git.

Some essays on the benefits of personal knowledge mapping are collected at [smsn-why](https://github.com/synchrony/smsn-why).

## Architecture

```
Clients                          Server                         Storage
---------------------------      -------------------------      ---------------------
smsn-mode (Emacs)          \                                    .smsn files (atoms)
Web UI (browser)        --- WebSocket ---> SmSn Server ---->    SQLite (index)
Chrome extension (clipboard)                (Jetty)             Lucene (full-text)
Python / scripts           /
```

The server is a standalone Java application. Clients connect over WebSocket (`ws://localhost:8182/smsn`) and issue actions (search, navigate, edit, import/export). Storage is file-based: each atom is a `.smsn` text file in a source directory, with SQLite and Lucene indexes for fast lookup. Indexes are derived from the source files and can be rebuilt at any time.

The data model is formally specified in Haskell using [Hydra](https://github.com/CategoricalData/hydra) and code-generated to immutable Java classes.

## Client Applications

### smsn-mode (Emacs)

The primary and most feature-complete client. An Emacs major mode for viewing, editing, searching, and navigating the knowledge graph.

Key capabilities:
- Tree view with adjustable depth (1-7 levels) and forward/backward navigation
- Full-text, acronym, and shortcut search
- Push/pull editing: edit the tree as text in a buffer, then push changes to the server
- Move submode for rapid single-key navigation
- Import and export in many formats (see [Export Formats](#export-formats))
- Configurable web search from note titles
- Hardware device integration (Typeatron chorded keyer)

Repository: [synchrony/smsn-mode](https://github.com/synchrony/smsn-mode)

### Web UI

A lightweight browser-based alternative. Vanilla JavaScript with no build step -- just open `web/index.html` in a browser.

Key capabilities:
- Tree view with expand/collapse and adjustable depth (1-4 levels)
- Full-text search
- Inline title editing, properties dialog, create/delete notes
- Split-pane view for cross-referencing
- Vim-style navigation (`j/k/h/l`) and Emacs-style chords (`C-c`, `C-x`)
- Cut/copy/paste, move up/down, undo

See `web/README.md` for keyboard shortcuts and full documentation.

### Chrome Extension

A bookmarking helper that formats the current tab's title and URL as a SmSn atom line and copies it to the clipboard. Paste into smsn-mode or the Web UI with `C-c y`. Special handling for Wikipedia URLs (converted to DBpedia IRIs).

## Getting Started

### Prerequisites

- **Java 11** or higher
- **Emacs 23+** (if using smsn-mode)
- A modern browser (if using the Web UI)

### 1. Build the Server

```bash
git clone https://github.com/synchrony/smsn.git
cd smsn
bin/build-server.sh
```

### 2. Set Up a Working Directory

The server runs from a **working directory** containing your `smsn.yaml` configuration. Create one anywhere on your system:

```bash
mkdir ~/smsn-data && cd ~/smsn-data
cp /path/to/smsn/example/smsn.yaml .
```

Edit `smsn.yaml` to point to your source directories (see `example/smsn.yaml` for a fully commented template). Create the source directories if they don't exist:

```bash
mkdir -p data/private data/personal data/public data/universal
```

### 3. Start the Server

From your working directory:

```bash
/path/to/smsn/bin/start-server.sh
```

On first startup, the server builds indexes from your `.smsn` files. This may take a few minutes for large graphs. The server listens on `0.0.0.0:8182` by default (override with `-p PORT` or `-h HOST`).

### 4. Connect

**With Emacs:** Install [smsn-mode](https://github.com/synchrony/smsn-mode), add to your Emacs config:

```elisp
(let ((default-directory "~/.emacs.d/elisp/"))
      (normal-top-level-add-subdirs-to-load-path))
(require 'smsn-mode)
```

Then `M-x smsn` to connect. Create your first note with `C-c n`.

**With the Web UI:** Open `web/index.html` in a browser. Press `/` to search or `C-c C-f` to find root notes.

See the [wiki howto](https://github.com/synchrony/smsn/wiki/What-you-need-to-know-to-use-Semantic-Synchrony) for a full introduction to using SmSn (no prior Emacs experience needed).

## Deployment

### Working Directory Layout

```
~/smsn-data/              # your working directory (any location)
    smsn.yaml             # server configuration
    index/                # SQLite + Lucene indexes (auto-created)
    activity.log          # activity log (optional)
    bk/                   # rolling index backups (created by cycle.sh)
    data/                 # source directories (or symlinks/absolute paths)
        private/          # .smsn files for private notes
        personal/
        public/
        universal/
```

Source directories can also live elsewhere and be referenced by absolute path in `smsn.yaml`. They can be Git repositories for version control and selective sharing.

### Helper Scripts

All scripts are in `bin/` and can be invoked from anywhere:

| Script | Description |
|--------|-------------|
| `bin/build-server.sh` | Build the server JAR |
| `bin/start-server.sh` | Start the server (builds if needed; run from working directory) |
| `bin/stop-server.sh` | Stop the server |
| `bin/cycle.sh` | Back up and clear the index for a fresh rebuild (run from working directory) |

For convenience, you can symlink `bin/` into your working directory:

```bash
ln -s /path/to/smsn/bin ~/smsn-data/bin
```

### Index Rebuild

To rebuild the index from `.smsn` source files (e.g., after external edits or to recover from corruption):

```bash
cd ~/smsn-data
bin/cycle.sh        # backs up the old index with rolling rotation
bin/start-server.sh # rebuilds automatically on startup
```

Or manually: stop the server, delete the index directory, and restart.

### Authentication

SmSn Server supports optional token-based authentication. Add to `smsn.yaml`:

```yaml
authentication:
  enabled: true
  token: "your-secret-token-here"
  publicSources:
    - public
    - universal
```

Clients authenticate by including the token in the WebSocket URL: `ws://localhost:8182/smsn?token=...`

See the [wiki](https://github.com/synchrony/smsn/wiki/SmSn-Server#authentication) for the full permission model.

### TLS via Reverse Proxy

The server does not provide TLS directly. For secure remote access, use a reverse proxy such as [Caddy](https://caddyserver.com/):

```
smsn.yourdomain.com {
    reverse_proxy /smsn localhost:8182
}
```

## Data Model

Each **atom** is stored as a plain-text `.smsn` file:

```
@id bxSoyLUM4w4RfitB
@title Arthur Dent
@alias http://dbpedia.org/resource/Arthur_Dent
@created 1344485170113
@source universal
@weight 0.75
@text ```
He's a jerk.
A real kneebiter.
```
* :QoTIPwLOID58u3Lr:
```

| Field | Description |
|-------|-------------|
| `id` | Globally unique 16-character Base62 identifier |
| `title` | The note's display text |
| `source` | Visibility tier (e.g., private, personal, public, universal) |
| `weight` | Importance, 0.0-1.0 (affects display brightness and search ranking) |
| `priority` | Optional TODO marker, 0.0-1.0 |
| `alias` | Optional URL or external IRI |
| `text` | Optional extended body text |
| `shortcut` | Optional quick-access name |
| `children` | Ordered list of child atom IDs (`* :id:` lines) |

**Sources** (visibility tiers) control sharing. Each source maps to a directory of `.smsn` files and is displayed in a distinct color. The conventional tiers are private (red), personal (yellow), public (green), and universal (blue), but you can define any number of sources.

## Export Formats

SmSn supports import and export in multiple formats via smsn-mode:

| Format | Import | Export |
|--------|--------|--------|
| Wiki / .smsn (native) | yes | yes |
| YAML | yes | yes |
| GraphML | yes | yes |
| Freeplane | yes | -- |
| Markdown | -- | yes |
| LaTeX | -- | yes |
| RDF (N-Triples) | -- | yes |
| Vertices TSV | -- | yes |
| Edges TSV | -- | yes |
| PageRank TSV | -- | yes |

## Legacy

SmSn previously used [Apache TinkerPop](https://tinkerpop.apache.org/) with Neo4j as its storage backend and Gremlin Server as the network layer. This was replaced in late 2025 with the current file-based architecture (standalone Jetty server + SQLite + Lucene). The legacy Docker images (`docker-smsn/`) reflect this older setup. For historical documentation, see the [Legacy TinkerPop Backend](https://github.com/synchrony/smsn/wiki/Legacy-TinkerPop-Backend) wiki page.

## Links

- [Wiki](https://github.com/synchrony/smsn/wiki) -- installation, user guide, server API, FAQ
- [smsn-mode](https://github.com/synchrony/smsn-mode) -- Emacs client
- [smsn-why](https://github.com/synchrony/smsn-why) -- essays on personal knowledge mapping
- [Video introduction](https://github.com/synchrony/smsn/wiki/A-video-introduction-to-Semantic-Synchrony) -- 45-minute course
- [Hydra](https://github.com/CategoricalData/hydra) -- code generation framework
- [Gitter](https://gitter.im/synchrony/Lobby) -- chat
- [Facebook](https://www.facebook.com/pg/semanticsynchrony/) -- community page
