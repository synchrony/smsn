package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.entities.Note;
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

        Note update;

        switch (getViewFormat()) {
            case json:
                update = parseJson(context);
                break;
            case wiki:
                update = parseWikiText(context);
                break;
            default:
                throw new IllegalStateException();
        }

        update.setTopic(getRoot().getTopic());

        // Apply the update
        Note n = context.getModel().view()
                .root(getRoot()).height(height).filter(getFilter()).style(style)
                .put(update)
                .get();
        try {
            addView(n, context);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        logUpdateOperation(context, n.getTopic());
    }

    private Note parseWikiText(final ActionContext params) {
        try {
            try (InputStream in = new ByteArrayInputStream(getView().getBytes())) {
                return params.getWikiParser().parse(in);
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    private Note parseJson(final ActionContext params) {
        try {
            return params.getJsonParser().parse(getView());
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
