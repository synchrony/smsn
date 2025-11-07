package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;

/**
 * A service for deriving a prioritized list of items in the knowledge base
 */
public class GetPriorities extends FilteredAction {

    private int maxResults = 100;

    public void setMaxResults(int maxResults) {
        if (maxResults <= 0) {
            throw new IllegalArgumentException(Params.MAX_RESULTS + " parameter must be a positive integer");
        }

        this.maxResults = maxResults;
    }

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException, BadRequestException {
        // Get priority queue (now uses Atoms)
        Queue<Atom> queue = context.getBrain().getPriorities().getQueue();

        // Filter and limit
        List<Atom> prioritizedAtoms = new ArrayList<>();
        int i = 0;
        for (Atom atom : queue) {
            if (getFilter() == null || context.getRepository().testFilter(atom, getFilter())) {
                prioritizedAtoms.add(atom);

                if (++i >= maxResults) {
                    break;
                }
            }
        }

        // Build list view
        TreeViewBuilder builder = new TreeViewBuilder(context.getRepository());
        net.fortytwo.smsn.brain.TreeNode tree = builder.buildListView(prioritizedAtoms, getFilter());

        try {
            JSONObject json = context.getTreeNodeJsonPrinter().toJson(tree);
            context.getMap().put(net.fortytwo.smsn.brain.Params.VIEW, json);
        } catch (java.io.IOException e) {
            throw new RequestProcessingException(e);
        }
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
