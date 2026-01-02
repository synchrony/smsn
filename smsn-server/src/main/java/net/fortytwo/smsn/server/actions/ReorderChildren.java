package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.util.ArrayList;
import java.util.List;

/**
 * A service for reordering children of a note.
 * Swaps two adjacent children without modifying any other properties.
 */
public class ReorderChildren extends FilteredAction {

    private AtomId parentId;
    private int fromIndex;
    private int toIndex;

    public void setParentId(AtomId parentId) {
        this.parentId = parentId;
    }

    public void setFromIndex(int fromIndex) {
        this.fromIndex = fromIndex;
    }

    public void setToIndex(int toIndex) {
        this.toIndex = toIndex;
    }

    private AtomId getParentId() {
        return notNull(parentId);
    }

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException, BadRequestException {
        AtomId pid = getParentId();

        // Load the parent atom
        Atom parent = context.getRepository().findById(pid).orElse(null);
        if (parent == null) {
            throw new BadRequestException("Parent atom not found: " + pid.value);
        }

        // Validate indices
        List<AtomId> children = parent.children;
        if (fromIndex < 0 || fromIndex >= children.size()) {
            throw new BadRequestException("Invalid fromIndex: " + fromIndex);
        }
        if (toIndex < 0 || toIndex >= children.size()) {
            throw new BadRequestException("Invalid toIndex: " + toIndex);
        }
        if (fromIndex == toIndex) {
            // Nothing to do
            context.getMap().put("parentId", pid.value);
            context.getMap().put("success", true);
            return;
        }

        // Get the child being moved
        AtomId childId = children.get(fromIndex);

        // Remove from old position and insert at new position
        context.getRepository().deleteChildAt(pid, fromIndex);
        context.getRepository().addChildAt(pid, childId, toIndex);

        context.getMap().put("parentId", pid.value);
        context.getMap().put("childId", childId.value);
        context.getMap().put("fromIndex", fromIndex);
        context.getMap().put("toIndex", toIndex);
        context.getMap().put("success", true);

        // Activity logging
        ActivityLog log = context.getActivityLog();
        if (log != null) {
            log.logSetPropertiesById(pid);
        }
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return true;
    }
}
