package model
{
[Bindable]
[RemoteClass(alias="net.fortytwo.myotherbrain.flashmob.model.FirstClassItemBean")]
public class FirstClassItem {
    public var subject:String;
    public var name:String;
    public var description:String;
    public var richTextDescription:String;
    public var icon:String;
    public var sensitivity:String;
    public var emphasis:Number;
    public var creationTimeStamp:Date;
    public var creationPlaceStamp:String;

    public function FirstClassItem() {
    }

    /*
         // Like a copy constructor...
         public function copy(other:Item):void {
             this.subject = other.subject;
             this.name = other.name;
             this.description = other.description;
             this.icon = other.icon;
             this.sensitivity = other.sensitivity;
             this.emphasis = other.emphasis;
             this.creationTimeStamp = other.creationTimeStamp;
             this.creationPlaceStamp = other.creationPlaceStamp;
         }*/
}
}