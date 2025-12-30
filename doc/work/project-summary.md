# Semantic Synchrony (SmSn) - Project Summary

*Last updated: December 2024*

## Overview

Semantic Synchrony (SmSn) is a personal knowledge graph system created in 2011. It enables users to build, navigate, and share interconnected notes in a graph structure, with a primary interface through Emacs (smsn-mode). The project combines a TinkerPop-based graph database backend with an Emacs major mode frontend, enabling fast, keyboard-driven knowledge management.

**Current Version:** 1.5 ("Four Hundred Pounds of Butterflies")

## Core Value Proposition

- **Graph-based note-taking**: Notes can have multiple parents, enabling non-linear organization
- **Fast traversal**: Views of 400k+ note graphs typically load in under 10ms
- **Selective sharing**: Four-tier visibility system (private, personal, public, universal)
- **Multi-format export**: GraphML, RDF, LaTeX, VCS (plain text), Markdown
- **Semantic Web compatible**: RDF export and SPARQL integration

## Architecture

### Components

| Component | Description |
|-----------|-------------|
| **smsn-mode** | Emacs major mode frontend ([separate repo](https://github.com/synchrony/smsn-mode)) |
| **smsn-server** | Gremlin Server plugin handling all actions (23 action classes) |
| **brain** | Core knowledge graph abstractions and I/O formats |
| **smsn-models** | Hydra-generated data classes from Haskell sources |
| **smsn-core** | Configuration and shared utilities |
| **smsn-rdf** | RDF vocabularies and semantic layer |
| **smsn-services** | P2P and OSC communication framework |

### Hardware Modules

These modules support specialized input devices and are intended to be kept in sync with the rest of the codebase:

| Module | Description |
|--------|-------------|
| **hand** | Extend-o-Hand: Bluetooth gestural data glove |
| **typeatron** | Monomanual Typeatron: Chorded keyboard with motion sensor |
| **monitron** | Omnisensory Monitron: Arduino-based sensor device |

### Technology Stack

- **Language**: Java 11
- **Build**: Gradle 8.x
- **Graph Database**: Apache TinkerPop 3.6.4 with Neo4j backend
- **Search**: Apache Lucene 3.6.2
- **RDF**: Sesame 4.1.2
- **Code Generation**: Hydra (Haskell to Java)
- **Protocol**: WebSocket (ws://localhost:8182/gremlin)

## Data Model

Each note (atom) has:
- **id**: 16-character base-62 unique identifier
- **title**: Human-readable text content
- **created**: Timestamp
- **weight**: 0.0-1.0 importance level (affects display and search ranking)
- **source**: Visibility tier (private/personal/public/universal)
- **priority**: Optional 0.0-1.0 TODO marker
- **alias**: Optional URL or external identifier
- **shortcut**: Optional quick-access name

Relationships are parent-child links forming a directed graph where notes can have multiple parents.

---

## Documentation Audit

### Current State

The wiki documentation is functional but shows its age:

| Document | Status | Issues |
|----------|--------|--------|
| Installation.md | Outdated | References Gradle 4.x syntax, old version numbers |
| Components.md | Outdated | References old repo names (joshsh/smsn vs synchrony/smsn) |
| FAQ.md | Current | Still accurate |
| What-you-need-to-know.md | Current | Good user guide |
| SmSn-Server.md | Current | Good API examples |
| Upgrading.md | Stale | Only covers 1.1 to 1.2 migration |
| Brainstorming.md | Active | Future ideas, some implemented |
| Data-model-design.md | Mixed | Describes 2.0 plans, some implemented |
| Video introduction | Unknown | YouTube links may be dated |

### Notes on Wiki

The wiki content is version-controlled in a separate repository, cloned here in `./wiki` for convenience. Updates should be made in the wiki repository.

### Missing Documentation

1. **No CONTRIBUTING.md** - Barrier to new contributors
2. **No architecture overview** - The system is complex; need high-level diagrams
3. **No API reference** - 23 server actions with no formal documentation
4. **No changelog** - No VERSION_HISTORY or CHANGELOG file
5. **smsn-models underdocumented** - Hydra workflow not well explained

---

## Modernization Opportunities

### High Priority

#### 1. Upgrade Dependencies

| Dependency | Current | Latest Stable | Notes |
|------------|---------|---------------|-------|
| TinkerPop | 3.6.4 | 3.7.x | Minor upgrade path |
| Lucene | 3.6.2 | 9.x | **Major gap** - consider migration |
| Sesame | 4.1.2 | Eclipse RDF4J 4.x | Sesame renamed to RDF4J |
| JUnit | 4.12 | 5.x | JUnit 5 migration |

**Lucene 3.6.2 is particularly concerning** - it's from 2012 and has known vulnerabilities. Modern Lucene (9.x) has significant API changes but much better performance and security.

#### 2. Build System Modernization

- Update to Gradle 8.x conventions (Kotlin DSL optional)
- Add dependency vulnerability scanning (e.g., OWASP Dependency-Check)
- Configure reproducible builds
- Add GitHub Actions CI/CD pipeline

#### 3. Documentation Overhaul

- Create `docs/` folder with MkDocs or similar static site generator
- Move wiki content into repository for better version control
- Add architecture decision records (ADRs)
- Generate API documentation from code

### Medium Priority

#### 4. Test Coverage - A Critical Concern

Current test infrastructure exists but coverage is thin relative to codebase size:

| Module | Source Files | Test Files | Ratio |
|--------|--------------|------------|-------|
| brain | 103 | 27 | 26% |
| smsn-server | 67 | 9 | 13% |
| typeatron | 36 | 1 | 3% |

**Total**: ~27,000 lines of source code with ~8,500 lines of test code.

The existing tests pass (`./gradlew test` succeeds), but coverage is concentrated in the `brain` module. This poses a significant risk for modernization efforts:

- **Dependency upgrades** (Lucene, TinkerPop, etc.) may silently break behavior
- **Hardware modules** have minimal test coverage, making changes risky
- **Server actions** are largely untested at the unit level

**Recommendations before major changes:**
- Add JaCoCo for coverage reporting to quantify gaps
- Prioritize tests for critical paths: search, view generation, tree updates
- Add integration tests for the 23 server actions
- Consider property-based testing for graph operations
- Add regression tests for specific use cases you rely on daily

#### 5. Code Modernization

- Migrate from anonymous classes to lambdas where applicable
- Consider records for DTOs (Java 16+)
- Add `Optional` usage consistency
- Reduce raw type usage in generics

#### 6. API Improvements

- Add OpenAPI/Swagger documentation for WebSocket API
- Consider REST API alongside WebSocket for broader client support
- Add versioning to API (currently implicit)

### Lower Priority

#### 7. Alternative Frontends

The Emacs dependency limits adoption. Consider:
- VS Code extension
- Web UI (could leverage existing WebSocket API)
- CLI tool for scripting

#### 8. Hardware Modules

The hand/typeatron/monitron modules are intended to remain part of the codebase. Key concerns:
- **Minimal test coverage** (typeatron has 1 test file for 36 source files)
- Need to ensure they continue to compile and function as core dependencies upgrade
- Arduino/embedded dependencies may need updates
- Consider adding basic smoke tests to catch regressions

#### 9. Docker Modernization

- Multi-stage builds for smaller images
- Docker Compose for full-stack development
- Health checks and proper signal handling

---

## Quick Wins

These improvements can be made with minimal risk:

1. **Update README.md** - Add badges, clearer getting-started section
2. **Add .editorconfig** - Ensure consistent formatting
3. **Add SECURITY.md** - Document vulnerability reporting process
4. **Clean up .gitignore** - Currently missing many patterns
5. **Add pre-commit hooks** - Formatting, linting
6. **Update Installation.md** - Fix Gradle commands for current version

---

## Breaking Changes to Consider

For a future 2.0 release:

1. **Lucene upgrade** - Will require index rebuilding
2. **Java 17+ baseline** - Enable modern language features
3. **TinkerPop 4.x** - When available, likely has breaking changes
4. **Simplified data model** - The 2.0 design doc has good ideas partially implemented

---

## Related Repositories

| Repository | Description |
|------------|-------------|
| [synchrony/smsn-mode](https://github.com/synchrony/smsn-mode) | Emacs frontend |
| [synchrony/docker-smsn](https://github.com/synchrony/docker-smsn) | Docker deployment |
| [synchrony/git-smsn](https://github.com/synchrony/git-smsn) | Git integration scripts |
| [synchrony/smsn-why](https://github.com/synchrony/smsn-why) | Essays on PKM philosophy |
| [synchrony/data-public](https://github.com/synchrony/data-public) | Shared public knowledge base |
| [CategoricalData/hydra](https://github.com/CategoricalData/hydra) | Code generation framework used by smsn-models |

---

## Strengths of the Current System

1. **Battle-tested**: Over a decade of active daily use
2. **Fast**: Sub-10ms views on large graphs
3. **Flexible data model**: Schema-free, emergent ontology
4. **Multi-user capable**: Git-based sharing with visibility tiers
5. **Export options**: Multiple formats including LaTeX for academic writing
6. **Semantic Web ready**: RDF export, SPARQL support

## Key Risks

1. **Bus factor**: Appears to be primarily maintained by one person
2. **Emacs dependency**: Limits potential user base
3. **Aging dependencies**: Security and maintenance concerns
4. **Documentation gaps**: Steep learning curve for contributors
5. **No CI/CD**: Manual release process

---

## Recommended Next Steps

### Phase 1: Foundation (Before Any Major Changes)

1. **Add CI/CD** - GitHub Actions for build verification on every push
2. **Add JaCoCo** - Get baseline coverage metrics
3. **Identify critical paths** - Document which features you use daily
4. **Write targeted regression tests** - Cover the critical paths before upgrading dependencies

### Phase 2: Stabilization

1. **Update wiki documentation** - Fix outdated Installation.md, Components.md
2. **Add CONTRIBUTING.md** - Lower barrier for potential contributors
3. **Investigate Lucene upgrade path** - This is the most pressing dependency issue

### Phase 3: Modernization

1. **Dependency upgrades** - With tests in place, upgrade Lucene, RDF4J, JUnit
2. **Java version bump** - Consider moving to Java 17 LTS
3. **Consider alternative frontends** - Web UI for broader adoption

---

*This document was generated by analyzing the SmSn codebase, wiki, and related documentation.*
