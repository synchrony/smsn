package model.concepts
{
    [Bindable]
    [RemoteClass(alias="net.fortytwo.myotherbrain.flashmob.model.FlashMOBAssociation")]
   	public class Association extends FirstClassItem
	{
		public var subjectURI:String;
    	public var objectURI:String;
    	
    	// Null <==> not specified
    	public var subject:FirstClassItem;
    	public var object:FirstClassItem;
    	
		public function Association()
		{
			super();
		}
		
	}
}