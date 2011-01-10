document.onmousemove = mouseMove;
document.onmouseup = mouseUp;

var mouseMoveCallback = null;
var stillMoving = "false";
var dragObject = null;
var mouseOffset = null;

function getMouseOffset(target, ev) {
    ev = ev || window.event;

    var docPos = getPosition(target);
    var mousePos = mouseCoords(ev);
    return {x:mousePos.x - docPos.x, y:mousePos.y - docPos.y};
}

function getPosition(e) {
    var left = 0;
    var top = 0;

    while (e.offsetParent) {
        left += e.offsetLeft;
        top += e.offsetTop;
        e = e.offsetParent;
    }

    left += e.offsetLeft;
    top += e.offsetTop;

    return {x:left, y:top};
}

function mouseMove(ev) {
    ev = ev || window.event;


    if (stillMoving == "true") {
        mouseMoveCallback(ev);
    }

    /*
     if (ev.button) {
     alert("aha! we caught a feisty little sheila!");
     dragObject = null;
     mouseMoveCallback = null;
     stillMoving = "false";
     }*/

    /*
     var mousePos = mouseCoords(ev);

     if (dragObject) {
     //alert("moving");
     dragObject.style.position = 'absolute';
     dragObject.style.top = mousePos.y - mouseOffset.y;
     dragObject.style.left = mousePos.x - mouseOffset.x;
     return false;
     } else {
     return true;
     }*/
}

function mouseCoords(ev) {
    if (ev.pageX || ev.pageY) {
        return {x:ev.pageX, y:ev.pageY};
    }
    return {
        x:ev.clientX + document.body.scrollLeft - document.body.clientLeft,
        y:ev.clientY + document.body.scrollTop - document.body.clientTop
    };
}

function mouseUp() {
    //alert("mouseup");
    dragObject = null;
    mouseMoveCallback = null;
    stillMoving = "false";
}

/*
 function makeDraggable(item) {
 if (!item) return;
 item.onmousedown = function(ev) {
 dragObject = this;
 mouseOffset = getMouseOffset(this, ev);
 return false;
 };
 }*/

MOB.ItemViewColonnade = function(app) {
    var root = document.createElement("div");
    root.setAttribute("class", "mob-item-view-colonnade");

    this.addColumn = function(column) {
        root.appendChild(column.getElement());

        // TODO: temporary
        root.appendChild(createDivider(column));
    };

    this.removeColumn = function(column) {
        root.removeChild(column.getElement());
        column.destroy();
    };

    this.getElement = function() {
        return root;
    };

    function createDivider(column) {
        var localMouseMoveCallback = function(ev) {
            //alert("local callback: " + JSON.stringify(ev));
            var dest = mouseCoords(ev);

            var before = origin.x;
            var after = dest.x;
            var diff = after - before;

            //app.setDebugMessage("before: " + before + "\n"
            //        + "after: " + after + "\n"
            //        + "--> offset: " + (after - before));
            //column.getElement().style.width = originalWidth + diff;
            //app.setDebugMessage("" + originalWidth + " + " + diff + " = " + (originalWidth + diff));
            app.setDebugMessage("width now: " + column.getElement().style.width);
        };

        var c = document.createElement("div");
        c.setAttribute("class", "mob-divider-column");

        var originalWidth;

        var h = document.createElement("div");
        c.appendChild(h);
        h.setAttribute("class", "mob-divider-handle");
        h.onmousedown = function(ev) {
            originalWidth = column.getElement().style.width;
            //alert("originalWidth: " + originalWidth);
            mouseMoveCallback = localMouseMoveCallback;
            stillMoving = "true";
            origin = mouseCoords(ev);
            //alert("got a mousedown event: " + JSON.stringify(ev));
            //dragObject = this;
            //mouseOffset = getMouseOffset(this, ev);
            //alert("mouse offset: " + JSON.stringify(mouseOffset));
            return false;
        };
        return c;
    }

    var origin;
};
