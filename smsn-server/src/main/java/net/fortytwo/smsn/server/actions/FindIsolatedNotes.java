package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

/**
 * A service for finding isolated notes (i.e. notes with no parents or children) in an Extend-o-Brain graph
 */
public class FindIsolatedNotes extends FilteredAction {

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException {
        List<Atom> isolated = new ArrayList<>();

        for (net.fortytwo.smsn.brain.AtomId atomId : context.getRepository().getAllAtomIds()) {
            Atom atom = context.getRepository().load(atomId);

            // Isolated means no children AND no parents
            if (atom.children.isEmpty() && context.getRepository().countParents(atomId) == 0) {
                if (getFilter() == null || context.getRepository().testFilter(atom, getFilter())) {
                    isolated.add(atom);
                }
            }
        }

        // Build a simple list view
        TreeViewBuilder builder = new TreeViewBuilder(context.getRepository());
        net.fortytwo.smsn.brain.TreeNode tree = builder.buildListView(isolated, getFilter());

        try {
            JSONObject json = context.getTreeNodeJsonPrinter().toJson(tree);
            context.getMap().put(net.fortytwo.smsn.brain.Params.VIEW, json);
        } catch (java.io.IOException e) {
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
