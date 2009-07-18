package model
{
public class MOB
{
    public static var NAMESPACE:String = "http://fortytwo.net/2009/05/myotherbrain#";

    public static const PERSONAL:String = NAMESPACE + "Personal";
    public static const PRIVATE:String = NAMESPACE + "Private";
    public static const PUBLIC:String = NAMESPACE + "Public";
      
    public static function getLocalName(mobURI:String):String {
        return mobURI.substring(MOB.NAMESPACE.length);
    }
}
}