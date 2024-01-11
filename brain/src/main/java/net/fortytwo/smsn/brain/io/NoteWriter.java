package net.fortytwo.smsn.brain.io;

import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.logging.Logger;

public abstract class NoteWriter {

    protected static final Logger logger = Logger.getLogger(NoteWriter.class.getName());

    public abstract List<Format> getFormats();

    public abstract void doWrite(Context context) throws IOException;

    public static class Context {
        private TopicGraph topicGraph;
        private KnowledgeBase knowledgeBase;
        private AtomId rootId;
        private Filter filter;
        private File destDirectory;
        private OutputStream destStream;
        private Format format;

        public KnowledgeBase getKnowledgeBase() {
            return knowledgeBase;
        }

        public void setKnowledgeBase(KnowledgeBase knowledgeBase) {
            this.knowledgeBase = knowledgeBase;
        }

        public TopicGraph getTopicGraph() {
            return topicGraph;
        }

        public void setTopicGraph(TopicGraph topicGraph) {
            this.topicGraph = topicGraph;
        }

        public AtomId getRootId() {
            return rootId;
        }

        public void setRootId(AtomId rootId) {
            this.rootId = rootId;
        }

        public Filter getFilter() {
            return filter;
        }

        public void setFilter(Filter filter) {
            this.filter = filter;
        }

        public OutputStream getDestStream() {
            return destStream;
        }

        public void setDestStream(OutputStream destStream) {
            this.destStream = destStream;
        }

        public File getDestDirectory() {
            return destDirectory;
        }

        public void setDestDirectory(File destDirectory) {
            this.destDirectory = destDirectory;
        }

        public Format getFormat() {
            return format;
        }

        public void setFormat(Format format) {
            this.format = format;
        }
    }

    protected void createDirectoryIfNotExists(final File dir) throws IOException {
        if (dir.exists()) {
            if (!dir.isDirectory()) {
                throw new IllegalArgumentException("file " + dir.getAbsolutePath() + " is not a directory");
            }
        } else {
            if (!dir.mkdirs()) {
                throw new IOException("could not create directory " + dir.getAbsolutePath());
            }
        }
    }

    protected  <E extends Exception> void timeAction(final String description,
                                                  final RunnableWithException<E> action) throws E {
        long before = System.currentTimeMillis();
        action.run();
        long after = System.currentTimeMillis();

        logger.info(description + " in " + (after - before) + " ms");
    }

    protected interface RunnableWithException<E extends Exception> {
        void run() throws E;
    }
}
