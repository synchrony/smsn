function copyTabToClipboard(tab, cachingId) {
    var text = "";

    if (0 == tab.url.indexOf("http://en.wikipedia.org/wiki/") || 0 == tab.url.indexOf("https://en.wikipedia.org/wiki/")) {
        var title = tab.title;
        var i = title.indexOf(" - ");
        if (i > 0) {
            title = title.substring(0, i);
        }

        var url = "http://dbpedia.org/resource/" + tab.url.substring(tab.url.indexOf("/wiki/") + 6);
        var j = url.indexOf("#");
        if (j > 0) {
            url = url.substring(0, j);
        }

        // Note: the extra indent saves on keystrokes when pasting the text into Brain-mode
        //       (when it is pasted to an indented position in the tree)
        text += "* " + title + "\n"
            + "                @sharability 1\n"
            + "                @alias " + url;
    } else {
        text += "* " + tab.title + " (web page)\n"
            + "                @alias " + tab.url;
    }

    if (null != cachingId) {
        text += "\n    * cached as " + cachingId;
    }

    var pre = document.getElementById("clipboard-view");
    pre.innerHTML = text;

    var input = document.getElementById("clipboard-input");
    // Make the textarea temporarily visible (otherwise, we can't copy)
    input.style.visibility = "visible";
    input.value = text;
    input.focus();
    input.select();
    document.execCommand("Copy");
    input.style.visibility = "hidden";

    document.getElementById("cache-button").onclick = function() {addToCache(tab)};
}

function useTab(tab) {
    copyTabToClipboard(tab, null);
}

function addToCache(tab) {
    var id = hex_md5(tab.url);



    copyTabToClipboard(tab, id);
}

// Note: you can't set the body element's "onload" in the HTML, due to Chrome security restrictions.
//       Neither can you execute a script inline from the HTML.
onload = function() {

    // Note: chrome.tabs.getCurrent doesn't work from a popup
    chrome.tabs.getSelected(null, useTab);
}
