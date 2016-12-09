package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.WriteGraphRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * A service for exporting an Extend-o-Brain graph to the file system
 */
public class WriteGraph extends Action<WriteGraphRequest> {

    @Override
    public String getName() {
        return "export";
    }

    @Override
    public void parseRequest(final WriteGraphRequest request, final RequestParams p) throws IOException {
        p.setFilter(request.getFilter());
        p.setFile(request.getFile());
        p.setFormat(request.getFormat());

        p.setRootId(request.getRoot());
        p.setHeight(request.getHeight());
    }

    @Override
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

    @Override
    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service (data is only written to the file system)
        return false;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
