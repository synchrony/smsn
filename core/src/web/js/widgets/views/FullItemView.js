MOB.FullItemView = function(item, app, onCloseCallback) {
    MOB.ItemView.call(this, item, app);
    var self = this;

    this.root.setAttribute("class", "mob-full-item-view");

    this.createContents = function() {
        var uri = this.item.uri;
        var name = this.item.name;
        var description = this.item.description;
        var icon = this.item.icon;

        var contents = document.createElement("div");

        var header = document.createElement("div");
        contents.appendChild(header);
        header.setAttribute("class", "mob-full-item-view-header");

        var headLeft = document.createElement("div");
        header.appendChild(headLeft);
        headLeft.setAttribute("class", "mob-full-item-view-header-left");
        //headLeft.appendChild(document.createTextNode(uri));
        
        if (null != name) {
            var nameEl = document.createElement("div");
            headLeft.appendChild(nameEl);
            nameEl.setAttribute("class", "mob-item-view-name");
            nameEl.appendChild(document.createTextNode(name));
        }

        var headSpacer = document.createElement("div");
        header.appendChild(headSpacer);
        headSpacer.setAttribute("class", "mob-full-item-view-header-spacer");

        var headRight = document.createElement("div");
        header.appendChild(headRight);
        headRight.setAttribute("class", "mob-full-item-view-header-right");

        var headRightAligned = document.createElement("div");
        headRight.appendChild(headRightAligned);
        headRightAligned.setAttribute("class", "mob-full-item-view-header-right-aligned");

        var sens = app.createSensitivityIcon(this.item);
        headRightAligned.appendChild(sens);
        sens.onclick = showMetadata;

        var close = document.createElement("div");
        headRightAligned.appendChild(close);
        close.setAttribute("class", "mob-close-icon");
        close.appendChild(document.createTextNode("[close]"));
        close.onclick = function() {
            //alert("closing...");
            onCloseCallback();
        };

        var body = document.createElement("div");
        contents.appendChild(body);

        if (null != icon) {
            var imageEl = document.createElement("div");
            body.appendChild(imageEl);
            imageEl.setAttribute("class", "mob-item-view-icon");
            var iconEl = document.createElement("img");
            imageEl.appendChild(iconEl);
            iconEl.setAttribute("class", "mob-item-view-icon-image");
            iconEl.setAttribute("src", icon);
        }

        if (null != description) {
            var descEl = document.createElement("div");
            body.appendChild(descEl);
            descEl.setAttribute("class", "mob-item-view-description");
            descEl.appendChild(document.createTextNode(description));
        }

        for (var i = 0; i < 2; i++) {
            var d = document.createElement("div");
            body.appendChild(d);
            d.appendChild(document.createTextNode("..."));
        }

        return contents;
    };

    function showMetadata() {
        var s = "uri: " + self.item.uri + "\n";

        if (null != self.item.sensitivity) {
            s += "sensitivity: " + self.item.sensitivity + "\n";
        }

        if (null != self.item.emphasis) {
            s += "emphasis: " + self.item.emphasis + "\n";
        }

        if (null != self.item.creationTimeStamp) {
            s += "time stamp: " + self.item.creationTimeStamp + "\n";
        }

        if (null != self.item.creationPlaceStamp) {
            s += "place stamp: " + self.item.creationPlaceStamp + "\n";
        }

        alert(s);
    }
};
