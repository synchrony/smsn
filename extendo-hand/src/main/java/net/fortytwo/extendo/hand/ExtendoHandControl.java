package net.fortytwo.extendo.hand;

import net.fortytwo.extendo.ExtendoDeviceControl;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.osc.OscReceiver;

import java.util.logging.Logger;

/**
 * A controller for the Extend-o-Hand gestural glove
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoHandControl extends ExtendoDeviceControl {
    protected static final Logger logger = Logger.getLogger(ExtendoHandControl.class.getName());

    public ExtendoHandControl(final OscReceiver oscReceiver,
                              final ExtendoAgent agent) throws DeviceInitializationException {
        super("/exo/hand", oscReceiver, agent);
    }


}
