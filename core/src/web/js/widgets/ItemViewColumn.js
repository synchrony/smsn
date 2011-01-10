MOB.ItemViewColumn = function() {
    var root = document.createElement("div");
    root.setAttribute("class", "mob-item-view-column");

    var body = document.createElement("body");
    root.appendChild(body);

    var views = document.createElement("div");
    body.appendChild(views);

    root.addItemView = function(view) {

    };

    root.removeItemView = function(view) {

    };

    root.setFooter = function(footer) {
        root.appendChild(footer);
    };

    return root;
};