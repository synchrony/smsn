package net.fortytwo.smsn.brain.io;

import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.logging.Logger;

public abstract class BrainWriter {
    protected static final Logger logger = Logger.getLogger(BrainWriter.class.getName());

    public abstract List<Format> getFormats();

    public abstract void doExport(Context context) throws IOException;

    public static class Context {
        private AtomGraph atomGraph;
        private KnowledgeBase knowledgeBase;
        private String rootId;
        private Filter filter;
        private File destDirectory;
        private OutputStream destStream;
        private Format format;

        private AtomGraph filteredGraph;

        public KnowledgeBase getKnowledgeBase() {
            return knowledgeBase;
        }

        public void setKnowledgeBase(KnowledgeBase knowledgeBase) {
            this.knowledgeBase = knowledgeBase;
        }

        public AtomGraph getAtomGraph() {
            return atomGraph;
        }

        public void setAtomGraph(AtomGraph atomGraph) {
            this.atomGraph = atomGraph;
        }

        public String getRootId() {
            return rootId;
        }

        public void setRootId(String rootId) {
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

        public AtomGraph getFilteredGraph() {
            if (null == filter) {
                return atomGraph;
            } else {
                if (null == filteredGraph) {
                    filteredGraph = atomGraph.createFilteredGraph(filter);
                }

                return filteredGraph;
            }
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

    protected void cleanDirectory(final File dir) throws IOException {
        FileUtils.cleanDirectory(dir);
    }
}
