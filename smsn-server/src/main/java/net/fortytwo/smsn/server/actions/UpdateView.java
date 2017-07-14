package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * A service for updating an Extend-o-Brain graph
 */
public class UpdateView extends RootedViewAction {

    @NotNull
    private String view;
    @NotNull
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

        TreeNode<Link> view;

        switch (getViewFormat()) {
            case json:
                view = parseJson(context);
                break;
            case wiki:
                view = parseWikiText(context);
                break;
            default:
                throw new IllegalStateException();
        }

        TreeViews.setId(view, getRoot().getId());

        // Apply the update
        context.getQueries().update(view, height, getFilter(), style);

        TopicGraph graph = context.getBrain().getTopicGraph();
        // TODO: produce an appropriate view (e.g. a search) if the root is null
        TreeNode<Link> n = null == getRoot()
                ? graph.createTopicTree(graph.createLink(null, null, Role.Noun))
                : context.getQueries().view(getRoot(), height, getFilter(), style);
        try {
            addView(n, context);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    private TreeNode<Link> parseWikiText(final ActionContext params) {
        try {
            try (InputStream in = new ByteArrayInputStream(getView().getBytes())) {
                return params.getWikiParser().parse(in).getContent();
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    private TreeNode<Link> parseJson(final ActionContext params) {
        try {
            return params.getJsonParser().parse(getView()).getContent();
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
