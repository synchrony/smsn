package
{
import update.actions.CreateAssociation;
import update.actions.CreateFirstClassItem;

import model.concepts.FirstClassItem;
import model.ItemRegistry;
import model.MOB;

import mx.controls.Alert;

public class StaticStuff
{
    public static var itemRegistry:ItemRegistry = new ItemRegistry();

    private static var sensitivityLevelToImageSource:Object = null;

    public function StaticStuff()
    {
    }

    public static function getSensitivityImageForItem(item:FirstClassItem):String {
        var prefix:String = "assets/icons/12x12/sensitivityLevels/";

        if (null == sensitivityLevelToImageSource) {
            sensitivityLevelToImageSource = {};
            sensitivityLevelToImageSource[MOB.PERSONAL] = prefix + "personal.png";
            sensitivityLevelToImageSource[MOB.PRIVATE] = prefix + "private.png";
            sensitivityLevelToImageSource[MOB.PUBLIC] = prefix + "public.png";
        }

        return null == item.sensitivity
                ? prefix + "none.png"
                : sensitivityLevelToImageSource[item.sensitivity];
    }

    public static function createNewItem():FirstClassItem {
        var it:FirstClassItem = new FirstClassItem();
        setDefaultValues(it);
        return it;
    }
    
    private static function setDefaultValues(it:FirstClassItem):void {
    	it.uri = mintURI();
        it.sensitivity = defaultSensitivity();
        it.emphasis = defaultEmphasis();
        it.creationTimeStamp = new Date();
    }
    
    private static function mintURI():String {
        // TODO: improve me (remove the decimal point, add numeric characters)
    	return FlashMOB.sessionInfo.baseURI + Math.random();	
    }

	private static function defaultEmphasis():Number {
		return 0.5;	
	}
	
	private static function defaultSensitivity():String {
		return MOB.PERSONAL;	
	}
	
    public static function createItemTmp(item:FirstClassItem):void {
        var a:CreateFirstClassItem = new CreateFirstClassItem();
        a.subject = item.uri;
        a.name = item.name;
        a.description = item.description;
        a.richTextDescription = item.richTextDescription;
        a.sensitivity = item.sensitivity;
        a.creationTimeStamp = item.creationTimeStamp;
        a.emphasis = item.emphasis;
Alert.show("a.subject = " + a.subject);

        FlashMOB.dispatcher.enqueueAction(a, null, null);
    }
    
    public static function updateItem(item:FirstClassItem):void {
    	// TODO
    }
    
    public static function associateTmp(subject:FirstClassItem, object:FirstClassItem):void {
 		var a:CreateAssociation = new CreateAssociation();
    	a.subject = mintURI();
        a.sensitivity = defaultSensitivity();
        a.emphasis = defaultEmphasis();
        a.creationTimeStamp = new Date();
        a.associationSubject = subject.uri;
 		a.associationObject = object.uri;
 		
 		FlashMOB.dispatcher.enqueueAction(a, null, null);   	
    }
}
}