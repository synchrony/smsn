package model {

import model.concepts.FirstClassItem;

public class ItemReference {
    public var item:FirstClassItem;
    public var referenceCount:Number;

    public function ItemReference(item:FirstClassItem) {
        this.item = item;
        this.referenceCount = 0;
    }
}
}