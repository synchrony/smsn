package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.view.TreeUpdater;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * A service for updating an Extend-o-Brain graph
 */
public class UpdateView extends RootedViewAction {

    private static final Logger logger = Logger.getLogger(UpdateView.class.getName());

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

        boolean verbose = SemanticSynchrony.getConfiguration().isVerbose();

        if (verbose) {
            logger.info("========== UpdateView Request ==========");
            logger.info("Format: " + getViewFormat());
            logger.info("Root ID: " + getRoot().id.value);
            logger.info("Height: " + height);
            logger.info("Incoming view content:\n" + getView());
        }

        // Load the actual root atom from the repository
        Atom rootAtom = context.getRepository().findById(getRoot().id).orElse(null);
        if (rootAtom == null) {
            throw new BadRequestException("Root atom not found: " + getRoot().id.value);
        }

        // Parse the view update to get the desired children and any top-level properties
        List<TreeNode> desiredChildren;
        TreeNode parsedTree = null;

        if (getView() == null || getView().trim().isEmpty()) {
            // Empty content means remove all children
            if (verbose) {
                logger.info("Empty view content - will remove all children");
            }
            desiredChildren = new ArrayList<>();
        } else {
            // Parse to get children and top-level properties
            switch (getViewFormat()) {
                case json:
                    parsedTree = parseJson(context);
                    break;
                case wiki:
                    parsedTree = parseWikiText(context);
                    break;
                default:
                    throw new IllegalStateException("Unknown format: " + getViewFormat());
            }

            if (verbose) {
                try {
                    JSONObject parsedJson = context.getTreeNodeJsonPrinter().toJson(parsedTree);
                    logger.info("Parsed tree:\n" + parsedJson.toString(2));
                } catch (IOException e) {
                    logger.warning("Failed to serialize parsed tree: " + e.getMessage());
                }
            }

            // The parser returns a synthetic root with the actual children
            desiredChildren = parsedTree.children;
        }

        // Merge parsed root properties with existing rootAtom properties
        // If a property was explicitly set in the parsed tree, use it; otherwise keep existing
        net.fortytwo.smsn.brain.Normed mergedWeight = rootAtom.weight;
        String mergedTitle = rootAtom.title;
        hydra.util.Opt<String> mergedAlias = rootAtom.alias;
        hydra.util.Opt<String> mergedText = rootAtom.text;
        hydra.util.Opt<String> mergedShortcut = rootAtom.shortcut;
        hydra.util.Opt<net.fortytwo.smsn.brain.Normed> mergedPriority = rootAtom.priority;

        if (parsedTree != null) {
            // Weight: use parsed if different from default (0.5)
            if (parsedTree.weight.value != SemanticSynchrony.DEFAULT_WEIGHT) {
                mergedWeight = parsedTree.weight;
            }

            // Title: use parsed if non-empty (dummy root has empty title by default)
            if (!parsedTree.title.isEmpty()) {
                mergedTitle = parsedTree.title;
            }

            // Optional properties: use parsed if present
            if (parsedTree.alias.isPresent()) {
                mergedAlias = parsedTree.alias;
            }
            if (parsedTree.text.isPresent()) {
                mergedText = parsedTree.text;
            }
            if (parsedTree.shortcut.isPresent()) {
                mergedShortcut = parsedTree.shortcut;
            }
            if (parsedTree.priority.isPresent()) {
                mergedPriority = parsedTree.priority;
            }
        }

        // Construct the tree with merged properties + desired children
        TreeNode treeUpdate = new TreeNode(
            rootAtom.id,
            rootAtom.created,
            mergedWeight,
            mergedPriority,
            rootAtom.source,
            mergedTitle,
            mergedAlias,
            mergedText,
            mergedShortcut,
            desiredChildren,
            desiredChildren.size(),
            0
        );

        if (verbose) {
            try {
                JSONObject updateJson = context.getTreeNodeJsonPrinter().toJson(treeUpdate);
                logger.info("TreeNode for update (root properties + parsed children):\n" + updateJson.toString(2));
            } catch (IOException e) {
                logger.warning("Failed to serialize update TreeNode: " + e.getMessage());
            }
        }

        // Adjust filter to inherit root's source for new children
        // This matches the original behavior where children inherit parent's source
        Filter originalFilter = getFilter();
        String minSource = originalFilter.getMinSource();
        if (minSource == null) {
            // Use the first source (usually "private") as the minimum if not specified
            minSource = SemanticSynchrony.getConfiguration().getSources().get(0).getName();
        }
        Filter adjustedFilter = new Filter(
            originalFilter.getMinWeight(),
            originalFilter.getDefaultWeight(),
            minSource,
            rootAtom.source.value  // Use root's source as default for new children
        );

        // Apply the update using TreeUpdater
        TreeUpdater updater = new TreeUpdater(context.getRepository(), context.getBrain().getActivityLog());
        updater.update(treeUpdate, height, adjustedFilter);

        // Generate the updated view
        TreeViewBuilder builder = new TreeViewBuilder(context.getRepository());
        TreeNode updatedView = builder.buildView(
            getRoot().id, height, getFilter(), getViewDirection());

        // Serialize and return the view
        try {
            JSONObject json = context.getTreeNodeJsonPrinter().toJson(updatedView);
            context.getMap().put(Params.VIEW, json);

            if (verbose) {
                logger.info("Response view:\n" + json.toString(2));
                logger.info("========== UpdateView Complete ==========");
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        addToHistory(getRoot().id);
    }

    private TreeNode parseWikiText(final ActionContext params) {
        try {
            try (InputStream in = new ByteArrayInputStream(getView().getBytes())) {
                // Parse directly using new TreeNodeWikiParser
                return params.getTreeNodeWikiParser().parse(in);
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
