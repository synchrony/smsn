Extendo is a collection of knowledge-based, wearable, and ubiquitous computing components which bring together the principles of [Humanistic Intelligence](http://en.wikipedia.org/wiki/Humanistic_intelligence) and the [Semantic Web](http://en.wikipedia.org/wiki/Semantic_Web).
It is a personal and research-oriented project,
and there currently isn't much documentation other than my PhD thesis (in progress).
However, here is an overview of the important stuff:

* **Extend-o-Brain**: a [personal knowledge base](http://en.wikipedia.org/wiki/Personal_knowledge_base) with a wiki interface.  It connects to a [Blueprints](https://github.com/tinkerpop/blueprints) graph database and includes an [Emacs](http://www.gnu.org/software/emacs/) front-end (think [Org-mode](http://en.wikipedia.org/wiki/Org-mode) with a [graph](http://en.wikipedia.org/wiki/Graph_%28mathematics%29)  of notes instead of a hierarchy).  Extend-o-Brain is the heart and soul of Extendo.  Or at least the brain.
* **Extendo RDF**: common vocabularies for use by Extendo agents, which communicate via RDF pub/sub
* **Extendo Server**: a collection of [Rexster](https://github.com/tinkerpop/rexster) extensions which connect Extend-o-Brain with the Emacs UI and the Brainstem.  Also contains the **Facilitator**, an event processor and message broker based on the [SesameStream](https://github.com/joshsh/sesamestream) continuous SPARQL query engine.
* **Extendo P2P**: a framework for service discover and streaming data interchange among Extendo agents
* **Omnisensory Monitron**: a semantic sensor framework and a specific device which combine sensor-based time series data (via [Arduino](http://www.arduino.cc/)) with controlled vocabularies and continuous queries.  See my presentation, [Semantics and Sensors](http://www.slideshare.net/joshsh/semantics-and-sensors).
* **Extend-o-Hand**: a glove-based gestural interface which reacts to simple baton gestures with low latency.  It communicates with the Brainstem via Bluetooth.
* **Monomanual Typeatron**: a particularly nifty wireless [chorded keyer](http://en.wikipedia.org/wiki/Chorded_keyboard) currently under development.  Part of the **Extend-o-Body** wearable computing platform.
* **Extendo Chrome Plugin**: bookmarks web pages into Extend-o-Brain and (optionally) logs web page visits into Extendo Events

See also:
* [Extendo Android](http://github.com/joshsh/extendo-android): contains the **Brainstem**, which controls and receives data from the Bluetooth devices, and a port of much of the Extendo stack to Android/Dalvik.  A brain in your pocket.
* [Extendo "extras"](https://github.com/joshsh/extendo-extras), which contain lightweight support for RFID tracking and speech recognition, among other things.

If you would like more details about Extendo, feel free to drop me a line at:

![Josh email](http://fortytwo.net/Home_files/josh_email.jpg)
