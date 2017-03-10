package net.fortytwo.smsn.brain.io;

import com.google.common.base.Preconditions;
import com.google.common.collect.Iterators;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.util.TypedProperties;
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

    private static final Logger logger = Logger.getLogger(BrainReader.class.getName());

    protected abstract void importInternal(Context context) throws IOException;

    private String defaultNodeName;

    public abstract List<Format> getFormats();

    private long transactionCounter = 0;
    private final int transactionBufferSize;

    protected BrainReader() {
        try {
            transactionBufferSize = SemanticSynchrony.getConfiguration().getInt(
                    SemanticSynchrony.TRANSACTION_BUFFER_SIZE, 0);
        } catch (TypedProperties.PropertyException e) {
            throw new IllegalStateException(e);
        }
    }

    public void doImport(
            File file, Format format, Brain brain, boolean recursive) throws IOException {

        assertFileExists(file);

        if (format.getType().equals(Format.Type.DirectoryBased)) {
            assertIsDirectory(file);
            importDirectoryNonrecursive(file, format, brain);
        } else {
            if (file.isDirectory()) {
                if (recursive) {
                    importDirectoryRecursive(file, format, brain);
                }
            } else {
                importSingleFile(file, format, brain);
            }
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

    protected synchronized void addToIndices(final Atom atom, final TopicGraph graph) {
        graph.reindexAtom(atom);

        checkAndCommit(graph);
    }

    private void checkAndCommit(final TopicGraph graph) {
        if (transactionBufferSize > 0 && transactionBufferSize == ++transactionCounter) {
            graph.commit();
            graph.begin();
            transactionCounter = 0;
        }
    }

    protected void assertFileExists(final File file) {
        Preconditions.checkArgument(file.exists(), "directory " + file.getAbsolutePath() + " does not exist");
    }

    protected void assertIsDirectory(final File dir) {
        Preconditions.checkArgument(dir.isDirectory(), "file " + dir.getAbsolutePath() + " is not a directory");
    }

    protected void assertDirectoryExists(final File dir) {
        assertFileExists(dir);
        assertIsDirectory(dir);
    }

    protected String getDefaultNodeName() {
        return defaultNodeName;
    }

    private void setDefaultNodeName(final String defaultNodeName) {
        this.defaultNodeName = defaultNodeName;
    }

    private long getSizeOf(final Context context) {
        return Iterators.size(context.getTopicGraph().getAllAtoms().iterator());
    }

    private void importDirectoryNonrecursive(File dir, Format format, Brain brain) throws IOException {
        Context context = new Context();
        context.setTopicGraph(brain.getTopicGraph());
        context.setSourceDirectory(dir);
        context.setFormat(format);

        doImport(context);
    }

    private void importDirectoryRecursive(File dir, Format format, Brain brain) throws IOException {
        Set<String> extensions = new HashSet<>();
        Collections.addAll(extensions, format.getFileExtensions());
        for (File file : dir.listFiles()) {
            if (!file.isHidden()) {
                if (file.isDirectory()) {
                    importDirectoryRecursive(file, format, brain);
                } else {
                    String ext = FilenameUtils.getExtension(file.getName());
                    if (extensions.contains(ext)) {
                        setDefaultNodeName(file.getName());
                        importSingleFile(file, format, brain);
                    }
                }
            }
        }
    }

    private void importSingleFile(File file, Format format, Brain brain) throws IOException {
        logger.info("importing file " + file);
        try (InputStream sourceStream = new FileInputStream(file)) {
            setDefaultNodeName(file.getName());

            Context context = new Context();
            context.setTopicGraph(brain.getTopicGraph());
            context.setSourceStream(sourceStream);
            context.setFormat(format);

            doImport(context);
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
