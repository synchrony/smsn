# Welcome!

Why would you want to keep or contribute to a knowledge graph? Check out [Praise for or Welcome to Knowledge Graphs](https://github.com/synchrony/praise-for-or-welcome-to-knowledge-graphs).

If you're new to Semantic Synchrony, check out [the wiki](https://github.com/synchrony/smsn/wiki).

# Components

Semantic Synchrony is a collection of interrelated projects in hardware and software for context-aware computing with semantics.  It contains:

* **Extend-o-Brain**: a [personal knowledge base](http://en.wikipedia.org/wiki/Personal_knowledge_base) with a wiki interface and [Semantic Web](http://en.wikipedia.org/wiki/Semantic_Web) interoperability.  It connects to a [TinkerPop](https://tinkerpop.apache.org/)-compatible graph database and includes an [Emacs](http://www.gnu.org/software/emacs/) major mode called [Brain-mode](https://github.com/joshsh/brain-mode) (think [Org-mode](http://en.wikipedia.org/wiki/Org-mode) with a [graph](http://en.wikipedia.org/wiki/Graph_%28mathematics%29) of notes instead of a hierarchy).  See [Extend-o-Brain installation](https://github.com/joshsh/smsn/wiki/Extend-o-Brain-installation).
* **Extend-o-Hand**: a Bluetooth-enabled gestural data glove with programming for handshakes, hand-offs (a virtual give-take interaction), and simple gestures including waves and taps.  An early version of Extend-o-Hand was used in a [keyboard-less typing system](http://fortytwo.net/share/qiJAriy/UbiKeyboard.mp4).
* **Monomanual Typeatron**: a 3D-printed wireless [chorded keyer](http://en.wikipedia.org/wiki/Chorded_keyboard) with integrated laser pointer, 6-axis motion sensor, and haptic outputs
* **Omnisensory Monitron**: a stationary [Arduino](http://www.arduino.cc/)-based device which gathers a variety of sensor data and produces a data stream using controlled vocabularies.  See [Semantics and Sensors](http://www.slideshare.net/joshsh/semantics-and-sensors)
* **SmSn Server**: a Gremlin script engine which connects Extend-o-Brain with its Emacs UI, the Brainstem, and any number of context-aware clients with each other through the [Stream42](https://github.com/joshsh/stream42) continuous SPARQL query engine and gestural services
* **SmSn P2P**: a simple framework for service discovery and data streaming using [JSON](http://json.org/) and [OSC](http://opensoundcontrol.org/)
* **SmSn RDF**: controlled [RDF](http://en.wikipedia.org/wiki/Resource_Description_Framework) vocabularies and utilities for modeling gestures and other events
* **SmSn Chrome Plugin**: a bookmark tool for use with Extend-o-Brain

See also:

* [SmSn Android](http://github.com/joshsh/extendo-android): contains the **Brainstem**, which controls and receives data from the Bluetooth devices, and a port of much of the Semantic Synchrony stack to Android/Dalvik
* [SmSn demos](https://github.com/joshsh/laboratory/tree/master/net/fortytwo/smsn/smsn-demos) a collection of demos, utilities, and scripts for analysis
* [SmSn "extras"](https://github.com/joshsh/extendo-extras), which contain lightweight support for RFID tracking and speech recognition, among other things
