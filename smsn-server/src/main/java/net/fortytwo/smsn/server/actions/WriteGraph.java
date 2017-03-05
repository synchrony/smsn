package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * A service for exporting an Extend-o-Brain graph to the file system
 */
public class WriteGraph extends IOAction {

    @NotNull
    private String rootId;

    public void setRootId(String rootId) {
        this.rootId = rootId;
    }

    @Override
    protected void performTransaction(final ActionContext params) throws RequestProcessingException, BadRequestException {

        BrainWriter.Context context = new BrainWriter.Context();
        context.setTopicGraph(params.getBrain().getTopicGraph());
        context.setKnowledgeBase(params.getBrain().getKnowledgeBase());
        context.setRootId(rootId);
        context.setFilter(filter);
        context.setFormat(format);
        BrainWriter writer = Format.getWriter(format);

        try {
            if (format.getType().equals(Format.Type.FileBased)) {
                try (OutputStream destStream = new FileOutputStream(file)) {
                    context.setDestStream(destStream);
                    writer.doExport(context);
                }
            } else {
                context.setDestDirectory(file);
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
