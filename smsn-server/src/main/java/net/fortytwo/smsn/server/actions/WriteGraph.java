package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;
import java.io.File;
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
        Format format = getFormat(p);

        BrainWriter.Context context = new BrainWriter.Context();
        context.setAtomGraph(p.getBrain().getAtomGraph());
        context.setKnowledgeBase(p.getBrain().getKnowledgeBase());
        context.setRootId(p.getRootId());
        context.setFilter(p.getFilter());
        context.setFormat(format);
        BrainWriter writer = Format.getWriter(format);

        File file = new File(p.getFile());

        try {
            if (format.getType().equals(Format.Type.FileBased)) {
                try (OutputStream destStream = new FileOutputStream(file)) {
                    context.setDestStream(destStream);
                    writer.doExport(context);
                }
            } else {
                if (file.exists()) {
                    if (!file.isDirectory()) {
                        throw new IllegalArgumentException("file " + file.getAbsolutePath() + " is not a directory");
                    }
                } else {
                    if (!file.mkdirs()) {
                        throw new RequestProcessingException("could not create directory " + file.getAbsolutePath());
                    }
                }
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

    private Format getFormat(final RequestParams p) {
        if (null == p.getFormat()) {
            throw new BadRequestException("format is required");
        }

        return Format.getFormat(p.getFormat());
    }
}
