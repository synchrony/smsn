package update.actions
{
[Bindable]
[RemoteClass(alias="net.fortytwo.myotherbrain.flashmob.update.actions.CreateAssociationBean")]
public class CreateAssociation extends Action {
    public var subject:String;
    public var name:String;
    public var description:String;
    public var richTextDescription:String;
    public var icon:String;
    public var sensitivity:String;
    public var emphasis:Number;
    public var creationTimeStamp:Date;
    public var creationPlaceStamp:String;
    public var associationSubject:String;
    public var associationObject:String;

    public function CreateAssociation() {
    }
}
}