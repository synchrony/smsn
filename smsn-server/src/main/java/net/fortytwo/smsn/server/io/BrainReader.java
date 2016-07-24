package net.fortytwo.smsn.server.io;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.TransactionalGraph;
import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomGraph;
import net.fortytwo.smsn.brain.Brain;
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

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class BrainReader {
    protected static final Logger logger = Logger.getLogger(BrainReader.class.getName());

    protected abstract void importInternal(Brain destBrain, InputStream sourceStream, Format format)
            throws IOException;

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

    public void doImport(final Brain destBrain, final InputStream sourceStream, final Format format)
            throws IOException {

        long before = System.currentTimeMillis();

        importInternal(destBrain, sourceStream, format);

        AtomGraph destGraph = destBrain.getAtomGraph();

        // note: we assume the graph is small
        commit(destGraph);

        reindexVertices(destGraph);

        // again, we assume the graph is small
        commit(destGraph);

        long after = System.currentTimeMillis();
        logger.info("imported " + format + " data in " + (after - before) + "ms");
    }

    protected String getDefaultNodeName() {
        return defaultNodeName;
    }

    public void setDefaultNodeName(final String defaultNodeName) {
        this.defaultNodeName = defaultNodeName;
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
            doImport(brain, sourceStream, format);
        }
    }

    private void reindexVertices(AtomGraph destGraph) {
        TransactionalGraph propertyGraph = (TransactionalGraph) destGraph.getPropertyGraph();
        for (Vertex v : propertyGraph.getVertices()) {
            String value = v.getProperty(SemanticSynchrony.VALUE);
            if (null != value) destGraph.indexForSearch(destGraph.getAtom(v), value);
        }
    }

    private void commit(final AtomGraph atomGraph) {
        Graph propertyGraph = atomGraph.getPropertyGraph();
        if (propertyGraph instanceof TransactionalGraph) {
            ((TransactionalGraph) propertyGraph).commit();
        }
    }
}
