package rpc
{
import actions.Action;

import model.FirstClassItem;

import mx.rpc.AsyncToken;
import mx.rpc.events.FaultEvent;
import mx.rpc.events.ResultEvent;
import mx.rpc.remoting.RemoteObject;

public class MOBDispatcher
{	
    [Bindable]
    private static var mobSession:RemoteObject = new RemoteObject();

    public function MOBDispatcher()
    {
        mobSession.destination = "flashmobsession";
    }

    ////////////////////////////////////
	
	public function enqueueAction(action:Action, handleResult:Function, handleFault:Function):void {
		var token:AsyncToken = mobSession.enqueueAction(action);			
		token.addResponder(new RPCHandler("enqueueAction", handleResult, handleFault));			
	}

	public function freeTextQuery(query:String, handleResult:Function, handleFault:Function):void {
		var token:AsyncToken = mobSession.freeTextQuery(query);	
		token.addResponder(new RPCHandler("freeTextQuery", handleResult, handleFault));			
	}
	
	public function getItems(handleResult:Function, handleFault:Function):void {
		var token:AsyncToken = mobSession.getItems();			
		token.addResponder(new RPCHandler("getItems", handleResult, handleFault));			
	}

	public function getObjectAssociations(subject:FirstClassItem, handleResult:Function, handleFault:Function):void {
		var token:AsyncToken = mobSession.getObjectAssociations(subject.subject);
		token.addResponder(new RPCHandler("getObjectAssociations", handleResult, handleFault));			
	}
		
	public function getSessionInfo(handleResult:Function, handleFault:Function):void {
		var token:AsyncToken = mobSession.getSessionInfo();			
		token.addResponder(new RPCHandler("getSessionInfo", handleResult, handleFault));			
	}
	
	public function setEmphasisThreshold(threshold:Number, handleResult:Function, handleFault:Function):void {
		var token:AsyncToken = mobSession.setEmphasisThreshold(threshold);	
		token.addResponder(new RPCHandler("setEmphasisThreshold", handleResult, handleFault));			
	}
	
	public function setVisibilityLevel(level:String, handleResult:Function, handleFault:Function):void {
		var token:AsyncToken = mobSession.setVisibilityLevel(level);	
		token.addResponder(new RPCHandler("setVisibilityLevel", handleResult, handleFault));			
	}
}
}