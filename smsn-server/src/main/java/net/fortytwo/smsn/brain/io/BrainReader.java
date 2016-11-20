package net.fortytwo.smsn.brain.io;

import com.google.common.collect.Iterators;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
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

    public void doImport(
            File fileOrDir, Format format, Brain brain, boolean recursive)
            throws IOException {

        if (!fileOrDir.exists()) {
            throw new IllegalArgumentException("file or directory not found: " + fileOrDir.getName());
        }

        if (fileOrDir.isDirectory()) {
            if (recursive) {
                importDirectoryRecursive(fileOrDir, format, brain);
            }
        } else {
            importSingleFile(fileOrDir, format, brain);
        }
    }

    public void doImport(Context context)
            throws IOException {

        long before = System.currentTimeMillis();

        AtomGraph destGraph = context.getAtomGraph();

        // note: we assume the graph is small
        //AtomGraph.wrapInTransaction(destGraph, () -> importInternal(context));
        //AtomGraph.wrapInTransaction(destGraph, () -> reindexVertices(destGraph));
        importInternal(context);
        reindexVertices(destGraph);

        long after = System.currentTimeMillis();
        logger.info("imported " + context.getFormat() + " data in " + (after - before) + " ms (before commit). " +
                "Resulting graph has " + getSizeOf(context) + " atoms");
    }

    protected String getDefaultNodeName() {
        return defaultNodeName;
    }

    public void setDefaultNodeName(final String defaultNodeName) {
        this.defaultNodeName = defaultNodeName;
    }

    private long getSizeOf(final Context context) {
        return Iterators.size(context.getAtomGraph().getAllAtoms().iterator());
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
            context.setAtomGraph(brain.getAtomGraph());
            context.setSourceStream(sourceStream);
            context.setFormat(format);

            doImport(context);
        }
    }

    private void reindexVertices(AtomGraph destGraph) {
        for (Atom a : destGraph.getAllAtoms()) {
            String value = a.getValue();
            if (null != value) destGraph.reindexAtom(a);
        }
    }

    public static class Context {
        private AtomGraph atomGraph;
        private InputStream sourceStream;
        private Format format;

        public AtomGraph getAtomGraph() {
            return atomGraph;
        }

        public void setAtomGraph(AtomGraph atomGraph) {
            this.atomGraph = atomGraph;
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
    }
}
