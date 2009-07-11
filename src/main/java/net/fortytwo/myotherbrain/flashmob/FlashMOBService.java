package net.fortytwo.myotherbrain.flashmob;

import net.fortytwo.myotherbrain.MyOtherBrain;
import org.apache.log4j.Logger;

/**
 * Author: josh
 * Date: Jul 10, 2009
 * Time: 7:56:48 PM
 */
public class FlashMOBService {
    private Logger LOGGER = MyOtherBrain.getLogger(FlashMOBService.class);
    
    public FlashMOBService() {
        System.out.println("FlashMOBService constructor called");
        LOGGER.info("FlashMOBService constructor called");
    }
    
    public String getVersionInfo() {
        System.out.println("getVersionInfo has been called.");
        return MyOtherBrain.getVersionInfo();
    }
}
