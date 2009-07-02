package net.fortytwo.myotherbrain.access;

import net.fortytwo.myotherbrain.MOBModel;

/**
 * Author: josh
 * Date: Jul 1, 2009
 * Time: 6:09:10 PM
 */
public class Session {
    private final String userName;
    private final MOBModel model;

    public Session(final String userName,
                   final MOBModel model) {
        this.userName = userName;
        this.model = model;
    }
    
    public MOBModel getModel() {
        return model;
    }
}
