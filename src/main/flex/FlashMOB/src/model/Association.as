package model
{
    [Bindable]
    [RemoteClass(alias="net.fortytwo.myotherbrain.flashmob.model.AssociationBean")]
   	public class Association extends FirstClassItem
	{
		public var associationSubject:String;
    	public var associationObject:String;
    	
		public function Association()
		{
			super();
		}
		
	}
}