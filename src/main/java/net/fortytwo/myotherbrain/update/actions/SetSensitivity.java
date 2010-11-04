package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.model.concepts.Atom;
import net.fortytwo.myotherbrain.model.concepts.SensitivityLevel;
import net.fortytwo.myotherbrain.update.UpdateException;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;

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
                          final WriteContext c) throws UpdateException {
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

    protected void executeUndo(final WriteContext c) throws UpdateException {
        Atom item = this.toThing(subject, Atom.class, c);
        item.setSensitivity(toThing(oldSensitivity, SensitivityLevel.class, c));
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        Atom item = this.toThing(subject, Atom.class, c);
        oldSensitivity = toURI(item.getSensitivity());
        item.setSensitivity(toThing(sensitivity, SensitivityLevel.class, c));
    }
}
