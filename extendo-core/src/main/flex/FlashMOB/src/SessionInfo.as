package
{
	[Bindable]
	[RemoteClass(alias="net.fortytwo.myotherbrain.flashmob.SessionInfo")]
	public class SessionInfo
	{
		public var userName:String;
	    public var versionInfo:String;
	    public var baseURI:String;
        public var sensitivityUpperBound:String;
        public var emphasisLowerBound:Number;
    
		public function SessionInfo()
		{
		}
	}
}