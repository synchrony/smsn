package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.view.TreeUpdate;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

        // Parse the view update to get the desired children and any top-level properties.
        // We use the parseUpdate(...) entry points so we get a TreeUpdate carrying the
        // side-channel (which root properties the user authored, which child atoms were
        // rendered as collapsed bullets). Without that, the updater can't tell the
        // difference between "user said nothing" and "user explicitly set the default".
        List<TreeNode> desiredChildren;
        TreeUpdate parsedUpdate = null;
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
                    parsedUpdate = parseJsonUpdate(context);
                    break;
                case wiki:
                    parsedUpdate = parseWikiTextUpdate(context);
                    break;
                default:
                    throw new IllegalStateException("Unknown format: " + getViewFormat());
            }
            parsedTree = parsedUpdate.getTree();

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

        // Merge parsed root properties with existing rootAtom properties.
        // The TreeUpdate side-channel tells us exactly which @<key> lines
        // the user authored at the top of the view, so we can replace the
        // brittle "treat default value as not specified" comparisons with
        // an honest check.
        Set<String> rootTouched = parsedUpdate != null
                ? parsedUpdate.getRootTouchedProperties()
                : new HashSet<>();
        net.fortytwo.smsn.brain.Normed mergedWeight = rootAtom.weight;
        String mergedTitle = rootAtom.title;
        hydra.util.Opt<String> mergedAlias = rootAtom.alias;
        hydra.util.Opt<String> mergedText = rootAtom.text;
        hydra.util.Opt<String> mergedShortcut = rootAtom.shortcut;
        hydra.util.Opt<net.fortytwo.smsn.brain.Normed> mergedPriority = rootAtom.priority;

        if (parsedTree != null) {
            if (rootTouched.contains(SemanticSynchrony.PropertyKeys.WEIGHT)) {
                mergedWeight = parsedTree.weight;
            }
            if (rootTouched.contains(SemanticSynchrony.PropertyKeys.TITLE)) {
                mergedTitle = parsedTree.title;
            }
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

        // Build a fresh TreeUpdate to pass to the updater. We rebase the
        // side-channel onto the root atom's actual id (the parser's
        // "dummy root" had no id), and reuse the per-atom touched sets
        // and collapsed sets for non-root atoms verbatim.
        Map<AtomId, Set<String>> mergedTouched = new HashMap<>();
        if (parsedUpdate != null) {
            mergedTouched.putAll(parsedUpdate.getTouchedProperties());
        }
        // Map root-touched property names onto the root atom's id.
        if (!rootTouched.isEmpty()) {
            mergedTouched
                    .computeIfAbsent(rootAtom.id, k -> new HashSet<>())
                    .addAll(rootTouched);
        }
        Set<AtomId> mergedCollapsed = parsedUpdate != null
                ? new HashSet<>(parsedUpdate.getCollapsedAtoms())
                : new HashSet<>();
        TreeUpdate updateToApply = new TreeUpdate(
                treeUpdate, rootTouched, mergedTouched, mergedCollapsed);

        // Adjust filter to inherit root's source for new children
        // This matches the original behavior where children inherit parent's source
        Filter effectiveFilter = getFilterFromContext(context);
        Filter adjustedFilter = new Filter(
            effectiveFilter.getMinWeight(),
            effectiveFilter.getDefaultWeight(),
            effectiveFilter.getIncludedSources(),
            rootAtom.source.value  // Use root's source as default for new children
        );

        // Apply the update using TreeUpdater
        TreeUpdater updater = new TreeUpdater(context.getRepository(), context.getActivityLog());
        updater.update(updateToApply, height, adjustedFilter);

        // Generate the updated view
        TreeViewBuilder builder = new TreeViewBuilder(context.getRepository());
        TreeNode updatedView = builder.buildView(
            getRoot().id, height, effectiveFilter, getViewDirection());

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

    private TreeUpdate parseWikiTextUpdate(final ActionContext params) {
        try {
            try (InputStream in = new ByteArrayInputStream(getView().getBytes())) {
                return params.getTreeNodeWikiParser().parseUpdate(in);
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    private TreeUpdate parseJsonUpdate(final ActionContext params) {
        try {
            return params.getTreeNodeJsonParser().parseUpdate(getView());
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
