package effects
{    
    import mx.controls.Alert;
    import mx.core.UIComponent;
    import mx.effects.EffectInstance;

    public class ItemRemoveEffectInstance extends EffectInstance
    {            
        public function ItemRemoveEffectInstance(targetObj:Object) {
            super(targetObj);
        }

        override public function play():void {
            //Alert.show("setting alpha of " + this.target + " to 0.0");
            (this.target as UIComponent).alpha = 0.0;
            super.play();
        }
        
        //override public function end():void  {
        //    //sndChannel.stop();
        //    super.end();
       // }
    }
}
