Extendo is a collection of interrelated projects in hardware and software which bring together the principles of [Humanistic Intelligence](http://en.wikipedia.org/wiki/Humanistic_intelligence) and the [Semantic Web](http://en.wikipedia.org/wiki/Semantic_Web).
It contains:

* **Extend-o-Brain**: a [personal knowledge base](http://en.wikipedia.org/wiki/Personal_knowledge_base) with a wiki interface.  It connects to a [Blueprints](https://github.com/tinkerpop/blueprints) graph database and includes an [Emacs](http://www.gnu.org/software/emacs/) front-end (think [Org-mode](http://en.wikipedia.org/wiki/Org-mode) with a [graph](http://en.wikipedia.org/wiki/Graph_%28mathematics%29)  of notes instead of a hierarchy).
* **Monomanual Typeatron**: a wireless [chorded keyer](http://en.wikipedia.org/wiki/Chorded_keyboard) with integrated laser pointer, 6-axis motion sensor, and haptic outputs
* **Omnisensory Monitron**: a stationary [Arduino](http://www.arduino.cc/)-based device which gathers a variety of sensor data and produces a data stream using controlled vocabularies.  See [Semantics and Sensors](http://www.slideshare.net/joshsh/semantics-and-sensors).
* **Extend-o-Hand**: a glove-based gestural interface which reacts to simple baton gestures with low latency.  It communicates with the Brainstem via Bluetooth.
* **Extendo Server**: a collection of [Rexster](https://github.com/tinkerpop/rexster) extensions which connect Extend-o-Brain with its Emacs UI and the Brainstem.  Also contains the **Facilitator**, an event processor and message broker based on the [SesameStream](https://github.com/joshsh/sesamestream) continuous SPARQL query engine.
* **Extendo P2P**: a simple framework for service discovery and data streaming
* **Extendo RDF**: common vocabularies for exchange of metadata in [RDF](http://en.wikipedia.org/wiki/Resource_Description_Framework)
* **Extendo Chrome Plugin**: bookmarks web pages into Extend-o-Brain and (optionally) logs web page visits via Extendo Server

See also:

* [Extendo Android](http://github.com/joshsh/extendo-android): contains the **Brainstem**, which controls and receives data from the Bluetooth devices, and a port of much of the Extendo stack to Android/Dalvik.
* [Extendo "extras"](https://github.com/joshsh/extendo-extras), which contain lightweight support for RFID tracking and speech recognition, among other things.

If you would like more details about Extendo, feel free to contact me at:

![Josh email](http://fortytwo.net/Home_files/josh_email.jpg)
