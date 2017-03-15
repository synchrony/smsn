package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.query.ViewStyle;
import net.fortytwo.smsn.server.errors.BadRequestException;

public abstract class BasicViewAction extends FilteredAction {
    protected int height;

    protected ViewStyle style = ViewStyle.FORWARD;

    public BasicViewAction() {
        super();
    }

    public void setHeight(int height) {
        this.height = height;

        if (height < 0) {
            throw new BadRequestException("height must be at least 0");
        }

        if (height > MAX_VIEW_HEIGHT) {
            throw new BadRequestException("height may not be more than 5");
        }
    }

    public void setStyle(final String styleName) {
        this.style = ViewStyle.lookupStyle(styleName);
    }
}
