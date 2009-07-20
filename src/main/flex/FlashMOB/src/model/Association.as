package model
{
    [Bindable]
    [RemoteClass(alias="net.fortytwo.myotherbrain.flashmob.model.AssociationBean")]
   	public class Association extends FirstClassItem
	{
		public var associationSubject:String;
    	public var associationObject:String;
    	
    	// Null <==> not specified
    	public var subjectBean:FirstClassItem;
    	public var objectBean:FirstClassItem;
    	
		public function Association()
		{
			super();
		}
		
	}
}