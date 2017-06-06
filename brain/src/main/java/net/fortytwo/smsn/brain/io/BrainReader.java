package net.fortytwo.smsn.brain.io;

import com.google.common.base.Preconditions;
import com.google.common.collect.Iterators;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.TopicGraph;
import org.apache.commons.io.FilenameUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

public abstract class BrainReader {

    protected static final Logger logger = Logger.getLogger(BrainReader.class.getName());

    protected abstract void importInternal(Context context) throws IOException;

    public abstract List<Format> getFormats();

    private long transactionCounter = 0;
    private final int transactionBufferSize;

    protected BrainReader() {
        transactionBufferSize = SemanticSynchrony.getConfiguration().getTransactionBufferSize();
    }

    public void doImport(
            File file, Format format, Brain brain) throws IOException {

        switch (format.getType()) {

            case Internal:
                break;
            case FileBased:
                break;
            case Complex:
                break;
            default:
                throw new IllegalStateException();
        }

        if (format.getType().equals(Format.Type.Complex)) {
            importComplex(format, brain);
        } else {
            Preconditions.checkNotNull(file);
            Preconditions.checkArgument(file.exists(), "file " + file.getAbsolutePath() + " does not exist");

            importFile(file, format, brain);
        }
    }

    public void doImport(Context context)
            throws IOException {

        long before = System.currentTimeMillis();

        importInternal(context);

        long after = System.currentTimeMillis();
        logger.info("imported " + context.getFormat() + " data in " + (after - before) + " ms (before commit). " +
                "Resulting graph has " + getSizeOf(context) + " atoms");
    }

    protected synchronized void checkAndCommit(final TopicGraph graph) {
        if (transactionBufferSize > 0 && transactionBufferSize == ++transactionCounter) {
            graph.commit();
            graph.begin();
            transactionCounter = 0;
        }
    }

    private void assertFileExists(final File file) {
        Preconditions.checkArgument(file.exists(), "directory " + file.getAbsolutePath() + " does not exist");
    }

    private void assertIsDirectory(final File dir) {
        Preconditions.checkArgument(dir.isDirectory(), "file " + dir.getAbsolutePath() + " is not a directory");
    }

    protected void assertDirectoryExists(final File dir) {
        assertFileExists(dir);
        assertIsDirectory(dir);
    }

    private long getSizeOf(final Context context) {
        return Iterators.size(context.getTopicGraph().getAllAtoms().iterator());
    }

    private void importComplex(final Format format, final Brain brain) throws IOException {
        Context context = new Context();
        context.setTopicGraph(brain.getTopicGraph());
        context.setFormat(format);

        doImport(context);
    }

    private void importDirectory(final File dir, final Format format, final Brain brain) throws IOException {
        Set<String> extensions = new HashSet<>();
        Collections.addAll(extensions, format.getFileExtensions());
        for (File file : dir.listFiles()) {
            if (!file.isHidden()) {
                if (file.isDirectory()) {
                    importDirectory(file, format, brain);
                } else {
                    String ext = FilenameUtils.getExtension(file.getName());
                    if (extensions.contains(ext)) {
                        importFile(file, format, brain);
                    }
                }
            }
        }
    }

    private void importFile(File file, Format format, Brain brain) throws IOException {
        logger.info("importing file " + file);
        if (file.isDirectory()) {
            importDirectory(file, format, brain);
        } else {
            try (InputStream sourceStream = new FileInputStream(file)) {
                Context context = new Context();
                context.setTopicGraph(brain.getTopicGraph());
                context.setSourceStream(sourceStream);
                context.setFormat(format);

                doImport(context);
            }
        }
    }

    public static class Context {
        private TopicGraph topicGraph;
        private InputStream sourceStream;
        private File sourceDirectory;
        private Format format;

        public TopicGraph getTopicGraph() {
            return topicGraph;
        }

        public void setTopicGraph(TopicGraph topicGraph) {
            this.topicGraph = topicGraph;
        }

        public InputStream getSourceStream() {
            return sourceStream;
        }

        public void setSourceStream(InputStream sourceStream) {
            this.sourceStream = sourceStream;
        }

        public Format getFormat() {
            return format;
        }

        public void setFormat(Format format) {
            this.format = format;
        }

        public File getSourceDirectory() {
            return sourceDirectory;
        }

        public void setSourceDirectory(File sourceDirectory) {
            this.sourceDirectory = sourceDirectory;
        }
    }
}
