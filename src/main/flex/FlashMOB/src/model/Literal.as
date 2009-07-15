package model
{
    [Bindable]
    [RemoteClass(alias="net.fortytwo.myotherbrain.flashmob.model.LiteralBean")]
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