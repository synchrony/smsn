package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.undo.UndoableAction;
import net.fortytwo.myotherbrain.MOBModelConnection;

import java.net.URI;
import java.util.Date;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class CreateFirstClassItem extends UndoableAction<MOBModelConnection> {
    private final String name;
    private final String description;
    private final URI icon;
    private final URI sensitivity;
    private final Float emphasis;
    private final Date creationTimeStamp;
    private final Float creationPlaceStampLongitude;
    private final Float creationPlaceStampLatitude;

    public CreateFirstClassItem(
            final String name,
            final String description,
            final URI icon,
            final URI sensitivity,
            final Float emphasis,
            final Date creationTimeStamp,
            final Float creationPlaceStampLongitude,
            final Float creationPlaceStampLatitude) {
        this.name = name;
        this.description = description;
        this.icon = icon;
        this.sensitivity = sensitivity;
        this.emphasis = emphasis;
        this.creationTimeStamp = creationTimeStamp;
        this.creationPlaceStampLongitude = creationPlaceStampLongitude;
        this.creationPlaceStampLatitude = creationPlaceStampLatitude;
    }

    protected void executeUndo(MOBModelConnection t) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    protected void executeRedo(MOBModelConnection t) {
        //To change body of implemented methods use File | Settings | File Templates.
    }
}