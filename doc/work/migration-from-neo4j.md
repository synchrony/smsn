# Migration from Neo4j/TinkerPop to File-Based Storage

## Overview

This document outlines the plan to remove Gremlin Server, TinkerPop, and Neo4j dependencies from SmSn, replacing them with a simpler file-based storage system that serves as a stepping stone toward full Hydra integration.

## Goals

1. **Simplify installation** - No external database servers required
2. **Preserve functionality** - CRUD, full-text search, tree views, import/export all work
3. **Fast startup** - No massive re-indexing on each server start
4. **Future-proof for Hydra** - Storage format and architecture compatible with eventual Hydra query/storage layer
5. **Git-friendly** - Human-readable files that work well with version control

## Architecture

### Current (TinkerPop/Neo4j)
```
Gremlin Server (WebSocket :8182)
    ↓
SmSnScriptEngine (custom Gremlin script engine)
    ↓
Action classes (GetView, Search, UpdateView, etc.)
    ↓
AtomRepository → GraphWrapper → Neo4j/TinkerGraph
    ↓
Neo4j embedded database + Lucene indices
```

### Target (File-Based)
```
SmSnServer (Jetty WebSocket :8182)
    ↓
ActionHandler (direct JSON dispatch)
    ↓
Action classes (unchanged)
    ↓
AtomRepository → FileBasedGraphWrapper
    ↓
YAML files (one per atom) + SQLite (ID index) + Lucene (full-text)
```

## Storage Design

### Atom Files (Wiki/SmSn format, then YAML)

**Phase 1: Wiki format (.smsn)** - Use existing VCS format for initial migration
- **Location**: `data/atoms/{source}/{id}.smsn`
- **Format**: Existing wiki-style format used by `AtomVCSReader`/`AtomVCSWriter`
- **Rationale**: No format conversion needed; existing import/export works immediately

**Phase 2: YAML format** - Migrate after server changes are stable
- **Location**: `data/atoms/{source}/{id}.yaml`
- **Format**: Hydra-compatible YAML (will use Hydra YAML coder in future)
- **Example**:
```yaml
id: "abc123"
created: 1704067200000
weight: 0.5
priority: null
source: "private"
title: "My Note Title"
alias: null
text: "Optional body text"
shortcut: null
children:
  - "def456"
  - "ghi789"
```

### Index Database (SQLite)
- **Location**: `data/index.db`
- **Purpose**: Fast ID lookups, shortcut lookups, metadata queries
- **Tables**:
  - `atoms(id TEXT PRIMARY KEY, source TEXT, created INTEGER, weight REAL, priority REAL, shortcut TEXT)`
  - `children(parent_id TEXT, child_id TEXT, position INTEGER)`
  - `shortcuts(shortcut TEXT PRIMARY KEY, atom_id TEXT)`

### Full-Text Index (Lucene)
- **Location**: `data/lucene-index/`
- **Indexed fields**: title, acronym (auto-generated)
- **Persistence**: Index survives restart; incremental updates on atom save

## Key Components to Create

### 1. FileBasedGraphWrapper
**Path**: `brain/src/main/java/net/fortytwo/smsn/brain/model/file/FileBasedGraphWrapper.java`

Replaces `GraphWrapper` with file-based storage:
- Implements same interface as current `GraphWrapper`
- Loads atoms from YAML files on demand (lazy loading with LRU cache)
- Writes atoms to YAML on commit
- Delegates indexing to `IndexManager`

### 2. IndexManager
**Path**: `brain/src/main/java/net/fortytwo/smsn/brain/model/file/IndexManager.java`

Manages SQLite + Lucene indices:
- SQLite for: ID lookups, shortcut lookups, child relationships
- Lucene for: full-text title search, acronym search
- Incremental updates (no full rebuild on startup)
- Version tracking to detect stale indices

### 3. SmSnServer
**Path**: `smsn-server/src/main/java/net/fortytwo/smsn/server/SmSnServer.java`

Standalone WebSocket server using Jetty:
- Same JSON protocol as current Gremlin Server integration
- Direct dispatch to Action classes
- No Gremlin dependency

### 4. SmSnServerMain
**Path**: `smsn-server/src/main/java/net/fortytwo/smsn/server/SmSnServerMain.java`

Application entry point:
- Parse command-line args / config file
- Initialize FileBasedGraphWrapper
- Start SmSnServer
- Handle shutdown gracefully

## Files to Modify

| File | Changes |
|------|---------|
| `brain/build.gradle` | Remove TinkerPop/Neo4j deps, add SQLite/Lucene/SnakeYAML |
| `smsn-server/build.gradle` | Remove gremlin-server, add jetty-websocket |
| `smsn-server/.../Action.java` | Update `createContext()` to use FileBasedGraphWrapper |
| `smsn-core/.../Configuration.java` | Add `dataDirectory`, `indexDirectory` properties |

## Files to Remove (Eventually)

| File | Reason |
|------|--------|
| `brain/.../model/pg/neo4j/Neo4jGraphWrapper.java` | Neo4j-specific |
| `brain/.../model/pg/tg/TinkerGraphWrapper.java` | TinkerGraph-specific |
| `smsn-server/.../SmSnScriptEngine.java` | Gremlin integration |
| `smsn-server/.../SmSnScriptEngineFactory.java` | Gremlin integration |
| `smsn-server/doc/gremlin-server-smsn.yaml` | Gremlin config |
| `smsn-server/doc/neo4j-smsn.properties` | Neo4j config |

## Dependencies

### Remove
```groovy
// brain/build.gradle
org.apache.tinkerpop:gremlin-core
org.apache.tinkerpop:tinkergraph-gremlin
org.apache.tinkerpop:neo4j-gremlin
org.neo4j:neo4j-tinkerpop-api-impl

// smsn-server/build.gradle
org.apache.tinkerpop:gremlin-server
```

### Add
```groovy
// brain/build.gradle
org.xerial:sqlite-jdbc:3.44.1.0
org.apache.lucene:lucene-core:9.9.1
org.apache.lucene:lucene-queryparser:9.9.1
org.yaml:snakeyaml:2.2

// smsn-server/build.gradle
org.eclipse.jetty.websocket:websocket-jetty-server:11.0.18
org.eclipse.jetty.websocket:websocket-jetty-api:11.0.18
```

## Migration Path for Existing Data

### From Neo4j
1. Start old server with Neo4j backend
2. Run `WriteGraph` action to export all atoms as YAML/VCS format
3. Stop old server
4. Start new server pointing to exported data directory
5. Indices are built automatically on first startup

### From VCS Format (already file-per-atom)
1. Point new server at existing VCS data directory
2. Start new server; indices built on startup
3. (Later) Convert `.smsn` files to `.yaml` format once server is stable

## Implementation Phases

### Phase 1: Core Storage Layer
1. Create `FileBasedGraphWrapper` using existing `AtomVCSReader`/`AtomVCSWriter` for .smsn files
2. Create `IndexManager` with SQLite + Lucene
3. Implement transaction semantics (write-ahead log or copy-on-write)
4. Unit tests for CRUD, search, child management

### Phase 2: Server Layer
1. Create `SmSnServer` with Jetty WebSocket
2. Create `SmSnServerMain` entry point
3. Wire up Action dispatch (reuse existing Action classes)
4. Integration tests with web client

### Phase 3: Dependency Cleanup
1. Remove TinkerPop/Neo4j/Gremlin from build files
2. Remove Neo4jGraphWrapper, TinkerGraphWrapper
3. Remove SmSnScriptEngine, SmSnScriptEngineFactory
4. Update documentation

### Phase 4: Testing & Migration
1. Data migration scripts (Neo4j → YAML, VCS → YAML)
2. Performance testing (startup time, search latency)
3. End-to-end testing with web UI

## Hydra Future

This architecture is designed as a stepping stone to full Hydra integration.

### Conceptual Alignment

A Hydra graph is a collection of **elements** (named bindings), similar to how SmSn is a collection of atoms indexed by ID. The key insight:

- **Hydra elements** = named bindings (like let-bindings in lambda calculus)
- **SmSn atoms** = named elements with properties and children
- **Top-level bindings** → individual files
- **Nested bindings** → inline within parent file (like inline children)

This means our file-per-atom structure naturally maps to Hydra's model: each atom is a top-level element/binding, and its children are references (not nested definitions). If Hydra later supports nested element definitions at arbitrary levels, our flat file structure remains compatible since children are stored as ID references.

### Migration Path to Full Hydra

1. **YAML format** - Hydra has YAML coders; current hand-written YAML can be replaced with generated coders
2. **Clean repository interface** - `AtomRepository` abstracts storage; Hydra storage could implement same interface
3. **Index abstraction** - `IndexManager` could later be replaced by Hydra query/reduction engine
4. **Type safety** - Already using Hydra-generated `Atom`, `TreeNode` data classes

When Hydra's query capabilities mature, the SQLite/Lucene layer can be replaced with:
- Hydra graph pattern queries for navigation
- Hydra reduction engine for computed properties
- Hydra coders for multi-format serialization (JSON, YAML, Protobuf, RDF)

### Why SQLite for Indexing Hydra Graphs (For Now)

Hydra's formal query model is based on graph patterns and lambda reduction, which is powerful but not yet optimized for:
- Fast random access by ID (needed for atom lookup)
- Full-text search over string properties (needed for search)
- Efficient child relationship queries (needed for tree traversal)

SQLite provides these capabilities with minimal complexity. As Hydra's query/reduction engine matures, it could subsume SQLite's role. The abstraction through `IndexManager` makes this swap straightforward.

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Data loss during migration | Always export before switching; keep Neo4j backup |
| Performance regression | LRU cache for hot atoms; index for all queries |
| Index corruption | Store index version; rebuild if version mismatch or corruption detected |
| Concurrent access | Single-writer pattern; file locks for safety |

## Questions Resolved

- **Database choice**: SQLite + Lucene (simplicity first)
- **Storage format**: YAML files (Hydra-compatible, human-readable)
- **Server stack**: Jetty WebSocket (proven, lightweight, already familiar)
