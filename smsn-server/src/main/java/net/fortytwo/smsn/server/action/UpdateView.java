package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.wiki.NoteReader;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.RootedViewRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.Principal;

/**
 * A service for updating an Extend-o-Brain graph
 */
public class UpdateView extends Action {

    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {
        UpdateRequest r;
        r = new UpdateRequest(request, p.getUser());

        p.setHeight(r.getHeight());
        // note: may be null
        p.setRootId(r.getRootId());
        p.setStyleName(r.getStyleName());
        p.setJsonView(r.jsonView);
        p.setWikiView(r.wikiView);
        p.setFilter(r.getFilter());
    }

    @Override
    public String getName() {
        return "update";
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Note rootNote;

        if (null != p.getWikiView()) {
            try {
                try (InputStream in = new ByteArrayInputStream(p.getWikiView().getBytes())) {
                    rootNote = p.getParser().fromWikiText(in);
                }
            } catch (IOException | NoteReader.NoteParsingException e) {
                throw new RequestProcessingException(e);
            }
        } else if (null != p.getJsonView()) {
            try {
                rootNote = p.getParser().fromJSON(p.getJsonView());
            } catch (JSONException e) {
                throw new RequestProcessingException(e);
            }
        } else {
            throw new IllegalStateException();
        }

        rootNote.setId(p.getRootId());

        // Apply the update
        p.getQueries().update(rootNote, p.getHeight(), p.getFilter(), p.getStyle());

        // TODO: produce an appropriate view (e.g. a search) if the root is null
        Note n = null == p.getRoot()
                ? new Note()
                : p.getQueries().view(p.getRoot(), p.getHeight(), p.getFilter(), p.getStyle());
        try {
            addView(n, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return true;
    }

    private class UpdateRequest extends RootedViewRequest {
        public final String wikiView;
        public final JSONObject jsonView;

        public UpdateRequest(final JSONObject json,
                             final Principal user) throws JSONException {
            super(json, user);

            // default to wiki-formatted updates, but support updates in either the JSON or wiki formats
            String view = this.json.getString(Params.VIEW);
            String viewFormat = this.json.optString(Params.VIEW_FORMAT);
            if (null == viewFormat || 0 == viewFormat.length() || viewFormat.equals(Params.WIKI_FORMAT)) {
                wikiView = view;
                jsonView = null;
            } else if (viewFormat.equals(Params.JSON_FORMAT)) {
                wikiView = null;
                jsonView = new JSONObject(view);
            } else {
                throw new JSONException("unexpected view format: " + viewFormat);
            }
        }
    }
}
