function copyTabToClipboard(tab, cachingId) {
    var text = "";

    if (tab.url.startsWith("http://en.wikipedia.org/wiki/") || tab.url.startsWith("https://en.wikipedia.org/wiki/")) {
        var title = tab.title.split(" - ")[0];

        var url = "http://dbpedia.org/resource/" + tab.url.substring(tab.url.indexOf("/wiki/") + 6);
        url = url.split("#")[0];

        text += "* " + title + "\n"
            + "                @source universal\n"
            + "                @alias " + url;
    } else {
        text += "* " + tab.title + " (web page)\n"
            + "                @alias " + tab.url;
    }

    if (cachingId) {
        text += "\n    * cached as " + cachingId;
    }

    var pre = document.getElementById("clipboard-view");
    pre.textContent = text;

    var input = document.getElementById("clipboard-input");
    input.style.visibility = "visible";
    input.value = text;
    input.focus();
    input.select();
    document.execCommand("Copy");
    input.style.visibility = "hidden";

    document.getElementById("cache-button").onclick = function() { addToCache(tab); };
}

function useTab(tab) {
    copyTabToClipboard(tab, null);
}

function addToCache(tab) {
    var id = hex_md5(tab.url);
    copyTabToClipboard(tab, id);
}

document.addEventListener('DOMContentLoaded', function () {
    chrome.tabs.query({ active: true, currentWindow: true }, function(tabs) {
        useTab(tabs[0]);
    });
});

