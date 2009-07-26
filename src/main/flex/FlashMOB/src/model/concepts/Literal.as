package model.concepts
{
[Bindable]
[RemoteClass(alias="net.fortytwo.myotherbrain.flashmob.model.FlashMOBLiteral")]
public class Literal extends FirstClassItem
{
    public var lexicalForm:String;
    public var datatypeURI:String;
    public var languageTag:String;

    public function Literal()
    {
        super();
    }
}
}