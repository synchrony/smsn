Semantic Synchrony CHANGELOG
========================================

SmSn 1.5 (Four Hundred Pounds of Butterflies)
----------------------------------------
****************************************
* Add dashboard pages for graph analytics, data sources, properties, the activity log, Git history, and search
* Create RStudio-based analytics dashboard
* Build a shadow jar for use with Gremlin Server
* Migrate from Maven to Gradle, adding a Gradle wrapper
* Improved parse error messages
****************************************

SmSn 1.4 (Pogo Stilts)
----------------------------------------
****************************************
* Refined Freeplane reader
* Made @title and @source optional properties
* Use generalized data sources configured on the server side
* Replaced @sharability with @source for data management and visibility
* Use YAML, rather than properties files, for configuration
* Use AND semantics by default in text search
* Added basic support for Git 'cycle'
* Exclude invisible atoms entirely from views, and skip them during updates
* Let visibility filters exclude nonexistent atoms
* Generalized data sources
* Added a Git history view action
* Use an inclusive criterion [min,inf) rather than an exclusive criterion (min,inf) in visibility filters
* Eliminated visibility maximums from filters
****************************************

SmSn 1.3 (Join the Army)
----------------------------------------
****************************************
* Support client connections via WebSocket
* Use a Base-62 character set [0-9A-Za-z] for IDs
* Additional validation in server actions
* Provide Markdown page views
* Added support for topics-and-pages data model
* Added parent and child counts to each atom in a view
* Added transaction buffering support for graph import
****************************************

SmSn 1.2 (Into the Future)
----------------------------------------
****************************************
* Added a VCS (version control system) graph reader and writer
* Added a command to populate a new, empty graph
* Cut down on graph index lookups through caching
* Added a ping action
* Migrated to Neo4j 2.3 and Lucene 3.6
* Migrated from TinkerPop 2.6 to Apache TinkerPop 3.2
****************************************

SmSn 1.1 (More Brains)
----------------------------------------
****************************************
* Created a unified BrainReader/BrainWriter framework for graph I/O
* Minimized TinkerPop2 dependencies
* Moved Brain-mode to a separate, GPL-licensed project
* Accepted first PRs to Brain-mode
* Added a Freeplane reader
* Made Brain-mode into an Emacs major mode
* Adapted to Java 8, Sesame 4, and RDF 1.1
* Updated the Monitron ontology and controller
* Added a LaTeX writer
* Added a GraphML reader
* Added "Brainstream" (eyes-free input) support
* Added support for shortcut-based search
* Added support for acronym-based search
* Added support for cooperative gesture recognition
* Added the UbiKeyboard application
* Added an RDF writer
* Created v2.0 of Extend-o-Brain inference support
* Added Linked Data and OSC support to the coordinator
* Added a Ripple environment for use with the Typeatron
****************************************

SmSn 1.0 (Semantic Gadgets)
----------------------------------------
****************************************
* Added a GraphML writer
* Integrated the Typeatron with Brain-mode via emacsclient
* Created a framework for inter-device discovery and communication
* Added type inference annotations to Brain-mode
* Added Extend-o-Brain support to the Brainstem
* Added an event processor to Extend-o-Brain
* Added a type inference and RDF export framework for Extend-o-Brain
* Added Bluetooth and OSC support to the devices
* Added support for single-device gesture recognition
* Created an Extend-o-Hand controller in Java
* Created a Typeatron controller in Java
* Created Max/MSP control panels for the devices
* Created Arduino-based libraries for the devices
* Created Extend-o-Hand
* Created the Monomanual Typeatron
****************************************

SmSn 0.x (Braaains)
----------------------------------------
****************************************
* Prototype design of the Monomanual Typeatron
* Added Emacspeak support to TinkerNotes (Brain-mode)
* Moved to an ordered, list-based data model in Extend-o-Brain
* Added RDF output to the Monitron
* Created the Omnisensory Monitron (a multi-sensor sampling device)
* Added a service to find graph roots
* Added a PageRank service for Extend-o-Brain
* Added Extend-o-Brain support for @alias URIs
* Added an activity log
* Added a Chrome bookmarking extension for use with Extend-o-Brain
* Made various deep changes to the data model, settling on a digraph model
* Added tab-separated writers for vertices and edges
* Created initial Extend-o-Brain services for view, update, history, and query
* Added a RESTful server for use with Emacs
* Created the TinkerNotes (Brain-mode) Emacs library
* Added storage, query and update support
* Implemented the initial "association graphs" data model
* Created pre- Extend-o-Brain applications, later moved to other repositories
* Created an Android library (later called the Brainstem)
* Created MyOtherBrain (Extend-o-Brain) prototypes with Java, Flash/Flex, and JavaScript
* Created a personal knowledge base ontology
****************************************
