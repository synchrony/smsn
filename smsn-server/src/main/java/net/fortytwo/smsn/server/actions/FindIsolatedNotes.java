package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for finding isolated notes (i.e. notes with no parents or children) in an Extend-o-Brain graph
 */
public class FindIsolatedNotes extends FilteredAction {

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException {
        TreeNode<Link> tree = context.getQueries().findIsolatedNotes(getFilter());
        try {
            addView(tree, context);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        context.getMap().put("title", "isolated notes");
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
