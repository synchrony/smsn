package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.util.TreeNodeConverter;
import net.fortytwo.smsn.brain.view.TreeUpdater;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * A service for updating an Extend-o-Brain graph
 */
public class UpdateView extends RootedViewAction {

    private String view;
    private Params.Format viewFormat;

    private String getView() {
        return notNull(view);
    }

    private Params.Format getViewFormat() {
        return notNull(viewFormat);
    }

    public void setView(String view) {
        this.view = view;
    }

    public void setViewFormat(Params.Format viewFormat) {
        this.viewFormat = viewFormat;
    }

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException, BadRequestException {
        super.performTransaction(context);

        // Parse the view update
        TreeNode treeUpdate;
        switch (getViewFormat()) {
            case json:
                treeUpdate = parseJson(context);
                break;
            case wiki:
                treeUpdate = parseWikiText(context);
                break;
            default:
                throw new IllegalStateException("Unknown format: " + getViewFormat());
        }

        // Ensure the tree has the root ID
        if (treeUpdate.id == null || !treeUpdate.id.equals(getRoot().id)) {
            treeUpdate = treeUpdate.withId(getRoot().id);
        }

        // Apply the update using TreeUpdater
        TreeUpdater updater = new TreeUpdater(context.getRepository(), context.getBrain().getActivityLog());
        updater.update(treeUpdate, height, getFilter());

        // Generate the updated view
        TreeViewBuilder builder = new TreeViewBuilder(context.getRepository());
        TreeNode updatedView = builder.buildView(
            getRoot().id, height, getFilter(), getViewDirection());

        // Serialize and return the view
        try {
            JSONObject json = context.getTreeNodeJsonPrinter().toJson(updatedView);
            context.getMap().put(Params.VIEW, json);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        addToHistory(getRoot().id);
    }

    private TreeNode parseWikiText(final ActionContext params) {
        try {
            try (InputStream in = new ByteArrayInputStream(getView().getBytes())) {
                // Parse using old WikiParser
                net.fortytwo.smsn.brain.model.entities.TreeNode<Link> oldTree =
                    params.getWikiParser().parse(in).getContent();

                // Convert to new TreeNode format
                return TreeNodeConverter.fromTreeNodeLink(oldTree);
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    private TreeNode parseJson(final ActionContext params) {
        try {
            // Parse directly using new TreeNodeJsonParser
            return params.getTreeNodeJsonParser().parse(getView());
        } catch (IOException e) {
            throw new RequestProcessingException(e);
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
