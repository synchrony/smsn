package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * A service for exporting an Extend-o-Brain graph to the file system
 */
public class WriteGraph extends FilteredAction {

    @NotNull
    private String format;
    @NotNull
    private String file;
    @NotNull
    private String root;
    private int height = 0;

    public String getFormat() {
        return format;
    }

    public String getFile() {
        return file;
    }

    public String getRoot() {
        return root;
    }

    public int getHeight() {
        return height;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public void setFile(String file) {
        this.file = file;
    }

    public void setRoot(String root) {
        this.root = root;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    @Override
    public void parseRequest(final RequestParams p) throws IOException {
        p.setFilter(getFilter());
        p.setFile(getFile());
        p.setFormat(getFormat());

        p.setRootId(getRoot());
        p.setHeight(getHeight());
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
