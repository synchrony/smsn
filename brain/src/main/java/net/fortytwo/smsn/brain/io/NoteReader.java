package net.fortytwo.smsn.brain.io;

import com.google.common.base.Preconditions;
import com.google.common.collect.Iterators;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.query.TreeViews;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.logging.Logger;

public abstract class NoteReader {

    protected static final Logger logger = Logger.getLogger(NoteReader.class.getName());

    protected abstract void importInternal(Context context) throws IOException;

    public abstract List<Format> getFormats();

    private long transactionCounter = 0;
    private final int transactionBufferSize;

    protected NoteReader() {
        transactionBufferSize = SemanticSynchrony.getConfiguration().getTransactionBufferSize();
    }

    public void doImport(Context context)
            throws IOException {

        long before = System.currentTimeMillis();

        importInternal(context);

        long after = System.currentTimeMillis();
        logger.info("imported " + context.getFormat() + " data in " + (after - before) + " ms (before commit). " +
                "Resulting graph has " + getSizeOf(context) + " notes");
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
        // Try to use AtomRepository if available (new API)
        if (context.getAtomRepository() != null) {
            return context.getAtomRepository().getAllAtomIds().size();
        }
        // Fall back to legacy TopicGraph API
        return Iterators.size(context.getTopicGraph().getAllNotes().iterator());
    }

    public static class Context {
        private TopicGraph topicGraph;
        private net.fortytwo.smsn.brain.repository.AtomRepositoryInterface atomRepository;
        private InputStream sourceStream;
        private File sourceDirectory;
        private Format format;
        private TreeViews queries;

        public TopicGraph getTopicGraph() {
            return topicGraph;
        }

        public void setTopicGraph(TopicGraph topicGraph) {
            this.topicGraph = topicGraph;
        }

        public net.fortytwo.smsn.brain.repository.AtomRepositoryInterface getAtomRepository() {
            return atomRepository;
        }

        public void setAtomRepository(net.fortytwo.smsn.brain.repository.AtomRepositoryInterface atomRepository) {
            this.atomRepository = atomRepository;
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

        public TreeViews getQueries() {
            return queries;
        }

        public void setQueries(TreeViews queries) {
            this.queries = queries;
        }
    }
}
