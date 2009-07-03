package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.SensitivityLevel;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetSensitivity extends WriteAction {
    private final URI subject;
    private final URI sensitivity;

    private URI oldSensitivity;

    public SetSensitivity(final URI subject,
                          final URI sensitivity) {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.sensitivity = sensitivity;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setSensitivity(toThing(oldSensitivity, SensitivityLevel.class, c));
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        oldSensitivity = toURI(item.getSensitivity());
        item.setSensitivity(toThing(sensitivity, SensitivityLevel.class, c));
    }
}
