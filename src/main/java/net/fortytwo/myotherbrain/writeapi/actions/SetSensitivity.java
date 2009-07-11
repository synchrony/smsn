package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.SensitivityLevel;
import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.writeapi.WriteAction;
import net.fortytwo.myotherbrain.writeapi.WriteContext;
import net.fortytwo.myotherbrain.writeapi.WriteException;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetSensitivity extends WriteAction {
    private final URI sensitivity;

    private URI oldSensitivity;

    public SetSensitivity(URI subject,
                          URI sensitivity,
                          final WriteContext c) throws WriteException {
        super(subject, c);

        if (null == sensitivity) {
            try {
                sensitivity = new URI(MOB.PERSONAL);
            } catch (URISyntaxException e) {
                throw new IllegalStateException();
            }
        } else {
            sensitivity = c.normalizeResourceURI(sensitivity);
        }

        this.sensitivity = sensitivity;
    }

    protected void executeUndo(final WriteContext c) throws WriteException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setSensitivity(toThing(oldSensitivity, SensitivityLevel.class, c));
    }

    protected void executeRedo(final WriteContext c) throws WriteException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        oldSensitivity = toURI(item.getSensitivity());
        item.setSensitivity(toThing(sensitivity, SensitivityLevel.class, c));
    }
}
