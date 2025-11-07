package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

/**
 * A service for finding root nodes of an Extend-o-Brain graph
 */
public class FindRoots extends BasicViewAction {

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException, BadRequestException {
        List<Atom> roots = new ArrayList<>();

        for (net.fortytwo.smsn.brain.AtomId atomId : context.getRepository().getAllAtomIds()) {
            Atom atom = context.getRepository().load(atomId);

            // Root notes have no parents
            if (context.getRepository().countParents(atomId) == 0) {
                if (getFilter() == null || context.getRepository().testFilter(atom, getFilter())) {
                    roots.add(atom);
                }
            }
        }

        // Build search results view with expanded children
        TreeViewBuilder builder = new TreeViewBuilder(context.getRepository());
        net.fortytwo.smsn.brain.TreeNode tree = builder.buildSearchResultsView(roots, height - 1, getFilter());

        try {
            JSONObject json = context.getTreeNodeJsonPrinter().toJson(tree);
            context.getMap().put(net.fortytwo.smsn.brain.Params.VIEW, json);
        } catch (java.io.IOException e) {
            throw new RequestProcessingException(e);
        }

        context.getMap().put("title", "all roots");
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
