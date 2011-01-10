var MOB = {};

MOB.Application = function() {
    var itemRegistry = new MOB.ItemRegistry();
    var root = document.createElement("div");
    root.setAttribute("class", "mob-application");
    var header = document.createElement("div");
    root.appendChild(header);
    header.setAttribute("class", "mob-header");

    var menuBar = new MOB.MenuBar(this);
    header.appendChild(menuBar.getElement());

    var body = document.createElement("div");
    root.appendChild(body);
    body.setAttribute("class", "mob-application-body");
    body.setAttribute("id", "appbody");


    var colonnade = new MOB.ItemViewColonnade(this);
    body.appendChild(colonnade.getElement());

    var footer = document.createElement("div");
    root.appendChild(footer);
    footer.setAttribute("class", "mob-application-footer");
    footer.appendChild(document.createTextNode("this is the footer"));
    var debugElement = document.createElement("div");
    footer.appendChild(debugElement);

    this.getElement = function() {
        return root;
    };

    this.getItemRegistry = function() {
        return itemRegistry;
    };

    this.getItemViewColonnade = function() {
        return colonnade;
    };

    this.createSensitivityIcon = function(item) {
        var icon = document.createElement("img");
        icon.setAttribute("class", "mob-sensitivity-icon");

        var sensitivity = item.sensitivity;
        var name = (null == sensitivity)
                ? "none"
                : sensitivity;
        icon.setAttribute("src", "images/sensitivityLevels/" + name + ".png");

        return icon;
    };

    this.setDebugMessage = function(msg) {
        while (debugElement.childNodes.length > 0) {
            debugElement.removeChild(debugElement.firstChild);
        }

        debugElement.appendChild(document.createTextNode(msg));
    };
};