Semantic Synchrony is a collection of interrelated projects in hardware and software for context-aware computing with semantics.  It contains:

* **Extend-o-Brain**: a [personal knowledge base](http://en.wikipedia.org/wiki/Personal_knowledge_base) with a wiki interface and [Semantic Web](http://en.wikipedia.org/wiki/Semantic_Web) interoperability.  It connects to a [Blueprints](https://github.com/tinkerpop/blueprints) graph database and includes an [Emacs](http://www.gnu.org/software/emacs/) front-end (think [Org-mode](http://en.wikipedia.org/wiki/Org-mode) with a [graph](http://en.wikipedia.org/wiki/Graph_%28mathematics%29) of notes instead of a hierarchy).
* **Extend-o-Hand**: a Bluetooth-enabled gestural data glove with programming for handshakes, hand-offs (a virtual give-take interaction), and simple gestures including waves and taps
* **Monomanual Typeatron**: a 3D-printed wireless [chorded keyer](http://en.wikipedia.org/wiki/Chorded_keyboard) with integrated laser pointer, 6-axis motion sensor, and haptic outputs
* **Omnisensory Monitron**: a stationary [Arduino](http://www.arduino.cc/)-based device which gathers a variety of sensor data and produces a data stream using controlled vocabularies.  See [Semantics and Sensors](http://www.slideshare.net/joshsh/semantics-and-sensors)
* **SmSn Server**: a collection of [Rexster](https://github.com/tinkerpop/rexster) extensions which connect Extend-o-Brain with its Emacs UI and the Brainstem, and any number of context-aware clients with each other through the [SesameStream](https://github.com/joshsh/sesamestream) continuous SPARQL query engine and gestural services
* **SmSn P2P**: a simple framework for service discovery and data streaming using [JSON](http://json.org/) and [OSC](http://opensoundcontrol.org/)
* **SmSn RDF**: controlled [RDF](http://en.wikipedia.org/wiki/Resource_Description_Framework) vocabularies and utilities for modeling gestures and other events
* **SmSn Chrome Plugin**: a bookmark tool for use with Extend-o-Brain

See also:

* [SmSn-Android](http://github.com/joshsh/smsn-android): contains the **Brainstem**, which controls and receives data from the Bluetooth devices, and a port of much of the Semantic Synchrony stack to Android/Dalvik
* [Smsn "extras"](https://github.com/joshsh/smsn-extras), which contain lightweight support for RFID tracking and speech recognition, among other things
