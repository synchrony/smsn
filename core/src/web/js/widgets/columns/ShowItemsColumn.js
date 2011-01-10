MOB.ShowItemsColumn = function(app, onCloseCallback) {
    var root = document.createElement("div");
    root.setAttribute("class", "mob-show-items-column");

    var body = document.createElement("div");
    root.appendChild(body);
    var viewStack = document.createElement("div");
    body.appendChild(viewStack);

    var footer = document.createElement("div");
    body.appendChild(footer);
    footer.setAttribute("class", "mob-item-view-column-footer");

    var close = document.createElement("div");
    footer.appendChild(close);
    close.setAttribute("class", "mob-close-icon");
    close.appendChild(document.createTextNode("[close]"));
    close.onclick = function() {
        //alert("closing...");
        onCloseCallback();
    };

    var views = {};

    function removeView(view) {
        delete views[view.getId()];
        viewStack.removeChild(view.getElement());
        view.destroy();
    }

    this.addItem = function(item) {
        var view;

        var onCloseCallback = function() {
            removeView(view);
        };

        view = new MOB.FullItemView(item, app, onCloseCallback);
        views[view.getId()] = view;

        viewStack.appendChild(view.getElement());
    };

    this.getElement = function() {
        return root;
    };

    this.destroy = function() {
        for (var id in views) {
            views[id].destroy();
        }
    };
};