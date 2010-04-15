package effects
{
	import effects.ItemRemoveEffectInstance;
    import mx.effects.Effect;
    import mx.effects.EffectInstance;
    import mx.effects.IEffectInstance;

   public class ItemRemoveEffect extends Effect
   {
       // Define constructor with optional argument.
       public function ItemRemoveEffect(targetObj:Object = null) {
           // Call base class constructor.
           super(targetObj);
           
           // Set instanceClass to the name of the effect instance class.
           instanceClass = ItemRemoveEffectInstance;         
       }
      
       // This effect modifies no properties, so your 
       // override of getAffectedProperties() method 
       // returns an empty array.
       override public function getAffectedProperties():Array {
           return ["alpha"];
       }
        
       //// Override initInstance() method.
       //override protected function initInstance(inst:IEffectInstance):void {
       //    super.initInstance(inst);
       //}   
   }
}
