MOB.MenuBar = function(app) {


    var testItem = {
        uri: "http://example.org/justATest",
        name: "Test Item",
        description: "This is a test.  This is only a test.",
        sensitivity: "public",
        icon: "http://icons.iconarchive.com/icons/iconshock/world-places/48/eiffel-tower-icon.png"
    };


    var root = document.createElement("div");
    root.setAttribute("class", "mob-menu-bar");

    function handleConnect() {

    }

    function handleCreateShowItemsColumn() {
        var colonnade = app.getItemViewColonnade();

        var c;
        c = new MOB.ShowItemsColumn(app, function() {
            colonnade.removeColumn(c);
        });
        colonnade.addColumn(c);
        c.addItem(testItem);
    }

    function handleCreateEditItemsColumn() {

    }

    function handleCreateSearchColumn() {

    }

    function handleSetVisibilityPublic() {

    }

    function handleSetVisibilityPersonal() {

    }

    function handleSetVisibilityPrivate() {

    }

    function handleSetEmphasisThreshold(threshold) {
        alert("TODO: set threshold to " + threshold);
    }

    function handleAboutMyOtherBrain() {

    }

    function handleUndo() {
        alert("undo");
    }

    function handleRedo() {

    }

    ////

    var nav = document.createElement("ul");
    root.appendChild(nav);
    nav.setAttribute("id", "nav");

    function createMenu(name) {
        var li = document.createElement("li");
        nav.appendChild(li);
        var a = document.createElement("a");
        li.appendChild(a);
        a.setAttribute("href", "#");
        a.appendChild(document.createTextNode(name));
        var ul = document.createElement("ul");
        li.appendChild(ul);
        return ul;
    }

    function createMenuItem(label, callback, menu) {
        var li = document.createElement("li");
        var a = document.createElement("a");
        li.appendChild(a);
        a.setAttribute("href", "#");
        a.onclick = callback;
        a.appendChild(document.createTextNode(label));
        menu.appendChild(li);
        return li;
    }

    function createSubmenu(name, menu) {
        var li = document.createElement("li");
        var a = document.createElement("a");
        li.appendChild(a);
        a.setAttribute("class", "daddy");
        a.setAttribute("href", "#");
        a.appendChild(document.createTextNode(name));
        var ul = document.createElement("ul");
        li.appendChild(ul);
        menu.appendChild(li);
        return ul;
    }

    var file = createMenu("File");
    var connect = createMenuItem("Connect", handleConnect, file);

    var edit = createMenu("Edit");
    var undo = createMenuItem("Undo", handleUndo, edit);
    undo.setAttribute("id", "mob-undo-menuitem");
    var redo = createMenuItem("Redo", handleRedo, edit);
    redo.setAttribute("id", "mob-redo-menuitem");

    var view = createMenu("View");
    var newColumn = createSubmenu("New column", view);
    var createShowItemsColumn = createMenuItem("Show", handleCreateShowItemsColumn, newColumn);
    var createEditItemsColumn = createMenuItem("Edit", handleCreateEditItemsColumn, newColumn);
    var createSearchColumn = createMenuItem("Search", handleCreateSearchColumn, newColumn);
    var setVis = createSubmenu("Set visibility", view);
    var setVisPublic = createMenuItem("Public", handleSetVisibilityPublic, setVis);
    var setVisPersonal = createMenuItem("Personal", handleSetVisibilityPersonal, setVis);
    var setVisPrivate = createMenuItem("Private", handleSetVisibilityPrivate, setVis);
    var setEmph = createSubmenu("Set emphasis", view);
    for (var i = 0; i < 6; i++) {
        var i = i;
        var e = createMenuItem("" + i, function() {
            handleSetEmphasisThreshold(i);
        }, setEmph);
    }

    var help = createMenu("Help");
    var about = createMenuItem("About MyOtherBrain", handleAboutMyOtherBrain, help);

    this.getElement = function() {
        return root;
    };

};