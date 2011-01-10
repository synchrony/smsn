package model
{
import mx.controls.Alert;
import model.concepts.FirstClassItem;

public class ItemRegistry
{
    private var referencesBySubject:Object = {};

    public function ItemRegistry()
    {
    }

    // TODO: decide what to do if the item *is* in the registry but its fields differ from those of the registered copy.
    public function registerItem(item:FirstClassItem):FirstClassItem {
        var r:ItemReference = this.referencesBySubject[item.uri] as ItemReference;

        if (null == r) {
            r = new ItemReference(item);
            this.referencesBySubject[item.uri] = r;
        }

        r.referenceCount++;
        return r.item;
    }

    public function unregisterItem(item:FirstClassItem):void {
        var r:ItemReference = this.referencesBySubject[item.uri] as ItemReference;

        if (null == r) {
            Alert.show("tried to unregister an item not in the registry: " + item);
        } else {
            r.referenceCount--;
            if (r.referenceCount < 1) {
                this.referencesBySubject[item.uri] = null;
            }
        }
    }

    public function size():Number {
        Alert.show("I don't know how to find the size of the registry, yet.");
        return NaN;
    }
}
}