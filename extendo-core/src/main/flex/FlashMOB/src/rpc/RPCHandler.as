package rpc
{
	import mx.controls.Alert;
	import mx.rpc.IResponder;
	
	public class RPCHandler implements IResponder
	{
		private var name:String;
		
		private var handleResult:Function;
		private var handleFault:Function;
		
		public function RPCHandler(name:String, handleResult:Function, handleFault:Function)
		{
			this.name = name;
			this.handleResult = handleResult;
			this.handleFault = handleFault;
		}

        public function result(data:Object):void {
  			if (null == this.handleResult) {
				//Alert.show("function " + name + " has no handler for result event: " + event);
			} else {
				this.handleResult(data);
			}      	
        }
	
		public function fault(info:Object):void {
			if (null == this.handleFault) {
				Alert.show("function " + this.name + " has no handler for fault event: " + info);
			} else {
				this.handleFault(info);	
			}			
		}
	}
}