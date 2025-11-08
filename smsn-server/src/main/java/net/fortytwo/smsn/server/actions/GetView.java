package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

/**
 * A service for retrieving hierarchical views of Extend-o-Brain graphs
 */
public class GetView extends RootedViewAction {

    @Override
    protected void performTransaction(final ActionContext context)
            throws RequestProcessingException, BadRequestException {
        super.performTransaction(context);

        // Use new TreeViewBuilder instead of old TreeViews
        TreeViewBuilder builder = new TreeViewBuilder(context.getRepository());
        net.fortytwo.smsn.brain.TreeNode tree = builder.buildView(
            getRoot().id, height, getFilter(), getViewDirection());

        // Serialize directly using new JSON printer
        try {
            JSONObject json = context.getTreeNodeJsonPrinter().toJson(tree);
            context.getMap().put(net.fortytwo.smsn.brain.Params.VIEW, json);
        } catch (java.io.IOException e) {
            throw new RequestProcessingException(e);
        }

        addToHistory(getRoot().id);
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
