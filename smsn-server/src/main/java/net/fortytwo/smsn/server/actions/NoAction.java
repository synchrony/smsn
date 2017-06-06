package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.Action;

/**
 * A no-op
 */
public class NoAction extends Action {

    @Override
    protected boolean doesRead() {
        return false;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
