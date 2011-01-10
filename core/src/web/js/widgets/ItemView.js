MOB.ItemView = function(item, app) {
    this.root = document.createElement("div");
    this.root.setAttribute("class", "mob-item-view");

    var contents = null;
    var self = this;
    var id = Math.random();

    function onChangedCallback(item) {
        self.item = item;
        contents = self.createContents();

        while (self.root.childNodes.length > 0) {
            self.root.removeChild(self.root.firstChild);
        }

        self.root.appendChild(contents);
    };

    app.getItemRegistry().subscribeToItem(item, id, onChangedCallback);

    this.createContents = function() {
        alert("Override createContents()");
    };

    this.getElement = function() {
        if (null == contents) {
            onChangedCallback(item);
        }

        return this.root;
    };

    this.getId = function() {
        return id;
    };

    this.destroy = function() {
        app.getItemRegistry().unsubscribeFromItem(item, id);
    };
};
