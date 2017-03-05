package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
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

    public void setView(String view) {
        this.view = view;
    }

    public void setViewFormat(Params.Format viewFormat) {
        this.viewFormat = viewFormat;
    }

    @Override
    protected void performTransaction(final ActionContext params) throws RequestProcessingException, BadRequestException {
        super.performTransaction(params);

        Note rootNote;

        switch (viewFormat) {
            case json:
                rootNote = parseJson(params);
                break;
            case wiki:
                rootNote = parseWikiText(params);
                break;
            default:
                throw new IllegalStateException();
        }

        rootNote.setId(root);

        // Apply the update
        params.getQueries().update(rootNote, height, filter, style);

        // TODO: produce an appropriate view (e.g. a search) if the root is null
        Note n = null == rootAtom
                ? new Note()
                : params.getQueries().view(rootAtom, height, filter, style);
        try {
            addView(n, params);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    private Note parseWikiText(final ActionContext params) {
        try {
            try (InputStream in = new ByteArrayInputStream(view.getBytes())) {
                return params.getWikiParser().parse(in);
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    private Note parseJson(final ActionContext params) {
        try {
            return params.getJsonParser().parse(view);
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
