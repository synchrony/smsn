package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.server.requests.FilteredResultsRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;

/**
 * A service for exporting an Extend-o-Brain graph to the file system
 */
public class WriteGraph extends Action {

    @Override
    public String getName() {
        return "export";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        WriteGraphRequest r = new WriteGraphRequest(request, p.getUser());

        p.setFilter(r.getFilter());
        p.setFile(r.file);
        p.setFormat(r.format);

        p.setRootId(r.rootId);
        p.setHeight(r.height);
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        if (null == p.getFormat()) {
            throw new BadRequestException("format is required");
        }

        BrainWriter.Context context = new BrainWriter.Context();
        context.setAtomGraph(p.getBrain().getAtomGraph());
        context.setKnowledgeBase(p.getBrain().getKnowledgeBase());
        context.setRootId(p.getRootId());
        context.setFilter(p.getFilter());
        context.setFormat(Format.getFormat(p.getFormat()));
        BrainWriter writer = Format.getWriter(context.getFormat());

        try {
            try (OutputStream destStream = new FileOutputStream(p.getFile())) {
                context.setDestStream(destStream);
                writer.doExport(context);
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service (data is only written to the file system)
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }

    private class WriteGraphRequest extends FilteredResultsRequest {
        private final String format;
        private final String file;

        private final String rootId;
        private final int height;

        public WriteGraphRequest(final JSONObject json,
                                 final Principal user) throws JSONException {
            super(json, user);

            format = this.json.getString(Params.FORMAT);
            file = this.json.getString(Params.FILE);

            rootId = this.json.optString(Params.ROOT);
            height = this.json.optInt(Params.HEIGHT, 0);
        }
    }
}
