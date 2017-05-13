## What it is
This is a bookmarking plugin for use with [Semantic Synchrony](https://github.com/synchrony/smsn) and [smsn-mode](https://github.com/synchrony/smsn-mode). After installing this directory as an [unpacked Chrome extension](https://developer.chrome.com/extensions/getstarted#unpacked), you will see the Semantic Synchrony icon (a red circle and a blue triangle connected by an S, plus a few more lines) in the corner of your browser.

Clicking on the icon will write a SmSn description of the webpage to your clipboard, e.g.

```
* Getting Started: Building a Chrome Extension - Google Chrome (web page)
                @alias https://developer.chrome.com/extensions/getstarted#unpacked
```

or

```
* Personal knowledge base
                @source "universal"
                @alias http://dbpedia.org/resource/Personal_knowledge_base
```

Wikipedia pages, like the one above, receive special treatment in SmSn, and can be visited with the shortcut `C-c C-t C-b w`.  All other notes with an HTTP `@alias` can be visited with the shortcut `C-c C-t C-a b` in smsn-mode. 

