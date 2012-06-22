function copyTabToClipboard(tab, cachingId) {
    var text = "* " + tab.title + " (web page)\n"
        + "    * " + tab.url;
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
