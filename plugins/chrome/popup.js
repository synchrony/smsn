document.addEventListener("DOMContentLoaded", () => {
    chrome.tabs.query({ active: true, currentWindow: true }, (tabs) => {
        if (tabs.length > 0) {
            let tab = tabs[0];

            document.getElementById("copy-button").addEventListener("click", () => {
                copyTabToClipboard(tab, null);
            });

            document.getElementById("cache-button").addEventListener("click", () => {
                addToCache(tab);
            });
        }
    });
});

function copyTabToClipboard(tab, cachingId) {
    let text = formatTabText(tab, cachingId);

    navigator.clipboard.writeText(text).then(() => {
        console.log("Copied to clipboard successfully!");
    }).catch(err => {
        console.error("Error copying to clipboard:", err);
    });

    document.getElementById("clipboard-view").innerHTML = text;
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

