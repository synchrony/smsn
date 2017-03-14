function httpGet(theUrl)
{
    var xmlHttp = null;

    xmlHttp = new XMLHttpRequest();
    xmlHttp.open( "GET", theUrl, false );
    xmlHttp.send( null );
    return xmlHttp.responseText;
}

/* Web page logging in SmSn is disabled for now
chrome.history.onVisited.addListener(function(result) {
    // This is an id for the visited page, not the event
    //var id = result.id;

    var url = result.url;
    var title = result.title;

    var serviceCallUrl = "http://localhost:8182/graphs/logging/smsn/webpage-visited"
        + "?url=" + encodeURIComponent(url);

    if (null != title) {
        serviceCallUrl += "&title=" + encodeURIComponent(title);
    }

    httpGet(serviceCallUrl);
});
*/
