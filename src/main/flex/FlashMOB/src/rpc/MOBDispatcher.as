package rpc
{
import actions.Action;

import mx.rpc.events.FaultEvent;
import mx.rpc.events.ResultEvent;
import mx.rpc.remoting.RemoteObject;
import mx.controls.Alert;

public class MOBDispatcher
{	
    [Bindable]
    private static var mobSession:RemoteObject = new RemoteObject();

	private var enqueueActionHandler:FunctionHandler = new FunctionHandler("enqueueAction");
	private var getItemsHandler:FunctionHandler = new FunctionHandler("getItems");
	private var getSessionInfoHandler:FunctionHandler = new FunctionHandler("getSessionInfo");
	private var setVisibilityLevelHandler:FunctionHandler = new FunctionHandler("setVisibilityLevel");
	
    public function MOBDispatcher()
    {
        mobSession.destination = "flashmobsession";

        mobSession.enqueueAction.addEventListener(ResultEvent.RESULT, enqueueActionHandler.handleResultEvent);
        mobSession.enqueueAction.addEventListener(FaultEvent.FAULT, enqueueActionHandler.handleFaultEvent);
  
        mobSession.getItems.addEventListener(ResultEvent.RESULT, getItemsHandler.handleResultEvent);
        mobSession.getItems.addEventListener(FaultEvent.FAULT, getItemsHandler.handleFaultEvent);

        mobSession.getSessionInfo.addEventListener(ResultEvent.RESULT, getSessionInfoHandler.handleResultEvent);
        mobSession.getSessionInfo.addEventListener(FaultEvent.FAULT, getSessionInfoHandler.handleFaultEvent);

		mobSession.setVisibilityLevel.addEventListener(ResultEvent.RESULT, setVisibilityLevelHandler.handleResultEvent);
		mobSession.setVisibilityLevel.addEventListener(FaultEvent.FAULT, setVisibilityLevelHandler.handleFaultEvent);
    }

    ////////////////////////////////////
	
	public function enqueueAction(action:Action, resultHandler:Function, faultHandler:Function):void {
		enqueueActionHandler.setHandlers(resultHandler, faultHandler);
		mobSession.enqueueAction(action);			
	}

	public function getItems(resultHandler:Function, faultHandler:Function):void {
		getItemsHandler.setHandlers(resultHandler, faultHandler);
		mobSession.getItems();			
	}

	public function getSessionInfo(resultHandler:Function, faultHandler:Function):void {
		getSessionInfoHandler.setHandlers(resultHandler, faultHandler);
		mobSession.getSessionInfo();			
	}
	
	public function setVisibilityLevel(level:String, resultHandler:Function, faultHandler:Function):void {
		setVisibilityLevelHandler.setHandlers(resultHandler, faultHandler);
		mobSession.setVisibilityLevel(level);	
	}
}
}