alert("About to load (2):" + document.getElementById('hello'))
console.log("About to load the popup script");

document.getElementById('copyButton').addEventListener('click', function() {
  console.log("Button clicked");
  alert("Button clicked...");

  browser.tabs.query({ active: true, currentWindow: true }, function(tabs) {
    console.log("Tabs queried:", tabs);

    var url = tabs[0].url;
    var title = tabs[0].title;
    var textToCopy = `You just bookmarked [${title}](${url})`;
    console.log("Text to copy:", textToCopy);

    browser.runtime.sendMessage({ textToCopy: textToCopy }, function(response) {
      console.log("Message sent to background script:", response);
      alert(response.message);
    });
  });
});


console.log("Popup script loaded");
