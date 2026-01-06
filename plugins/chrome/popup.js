document.addEventListener("DOMContentLoaded", () => {
    chrome.tabs.query({ active: true, currentWindow: true }, (tabs) => {
        if (tabs.length > 0) {
            let tab = tabs[0];

            // Immediately copy JSON to clipboard for SmSn Web UI paste functionality
            copyJsonToClipboard(tab);

            // Display the wiki format in the popup
            document.getElementById("clipboard-view").innerHTML = formatTabText(tab, null);

            document.getElementById("copy-button").addEventListener("click", () => {
                copyWikiToClipboard(tab, null);
            });

            document.getElementById("cache-button").addEventListener("click", () => {
                addToCache(tab);
            });
        }
    });
});

// Extract bookmark data from a browser tab
function extractBookmarkData(tab) {
    let data = {
        type: "smsn-bookmark",
        title: tab.title,
        url: tab.url
    };

    // Check for Wikipedia - use DBpedia alias and universal source
    if (tab.url.startsWith("http://en.wikipedia.org/wiki/") || tab.url.startsWith("https://en.wikipedia.org/wiki/")) {
        let title = tab.title.split(" - ")[0];
        data.title = title;

        let alias = "http://dbpedia.org/resource/" + tab.url.substring(tab.url.indexOf("/wiki/") + 6);
        let j = alias.indexOf("#");
        if (j > 0) {
            alias = alias.substring(0, j);
        }
        data.alias = alias;
        data.source = "universal";
    } else {
        // Regular web page - append "(web page)" to title and use URL as alias
        data.title = tab.title + " (web page)";
        data.alias = tab.url;
    }

    return data;
}

// Copy JSON bookmark data to clipboard (for SmSn Web UI)
function copyJsonToClipboard(tab) {
    let data = extractBookmarkData(tab);
    let json = JSON.stringify(data);

    navigator.clipboard.writeText(json).then(() => {
        console.log("JSON copied to clipboard for Web UI");
    }).catch(err => {
        console.error("Error copying JSON to clipboard:", err);
    });
}

// Copy wiki format to clipboard (for Emacs)
function copyWikiToClipboard(tab, cachingId) {
    let text = formatTabText(tab, cachingId);

    navigator.clipboard.writeText(text).then(() => {
        console.log("Wiki format copied to clipboard");
        // Update button text temporarily
        let btn = document.getElementById("copy-button");
        btn.textContent = "Copied!";
        setTimeout(() => { btn.textContent = "Copy wiki format"; }, 1500);
    }).catch(err => {
        console.error("Error copying to clipboard:", err);
    });

    document.getElementById("clipboard-view").innerHTML = text;
}

// Legacy function name
function copyTabToClipboard(tab, cachingId) {
    copyWikiToClipboard(tab, cachingId);
}

function formatTabText(tab, cachingId) {
    let text = "";

    if (tab.url.startsWith("http://en.wikipedia.org/wiki/") || tab.url.startsWith("https://en.wikipedia.org/wiki/")) {
        let title = tab.title.split(" - ")[0];
        let url = "http://dbpedia.org/resource/" + tab.url.substring(tab.url.indexOf("/wiki/") + 6);
        let j = url.indexOf("#");
        if (j > 0) {
            url = url.substring(0, j);
        }

        text += `* ${title}\n                @source universal\n                @alias ${url}`;
    } else {
        text += `* ${tab.title} (web page)\n                @alias ${tab.url}`;
    }

    if (cachingId) {
        text += `\n    * cached as ${cachingId}`;
    }

    return text;
}

function addToCache(tab) {
    crypto.subtle.digest("SHA-256", new TextEncoder().encode(tab.url))
        .then(hashBuffer => {
            let hashArray = Array.from(new Uint8Array(hashBuffer));
            let id = hashArray.map(byte => byte.toString(16).padStart(2, '0')).join('');
            copyTabToClipboard(tab, id);
        });
}
