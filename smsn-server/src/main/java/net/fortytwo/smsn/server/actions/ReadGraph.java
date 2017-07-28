package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.apache.tinkerpop.gremlin.structure.Graph;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * A service for importing an Extend-o-Brain subgraph
 */
public class ReadGraph extends IOAction {
    private static final Map<Graph, Set<String>> importsInProgress = new HashMap<>();
    private static final Map<Graph, Set<String>> importsSucceeded = new HashMap<>();

    private static final String DEFAULT = "default";

    @Override
    protected void performTransaction(final ActionContext params) throws RequestProcessingException, BadRequestException {
        if (null == getFormat()) {
            throw new BadRequestException("format is required");
        }

        NoteReader reader = Format.getReader(getFormat());

        String lockId = findLockId();
        beginImport(params.getGraphWrapper().getGraph(), lockId);

        boolean success = false;
        try {
            reader.doImport(getFile(), getFormat(), params.getBrain());
            success = true;
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        } finally {
            finishImport(params.getGraphWrapper().getGraph(), lockId, success);
        }
    }

    @Override
    protected boolean doesRead() {
        // doesn't expose data which has been read from the graph,
        // although data is loaded into the graph from the file system
        return false;
    }

    @Override
    protected boolean doesWrite() {
        // this operation does modify the graph
        return true;
    }

    private static synchronized void beginImport(final Graph g, final String lockId) throws BadRequestException {
        Set<String> files = importsSucceeded.get(g);
        if (null != files && files.contains(lockId)) {
            throw new BadRequestException("graph at " + lockId + " has already been imported successfully");
        }

        files = importsInProgress.get(g);
        if (null != files) {
            if (files.contains(lockId)) {
                throw new BadRequestException("graph at " + lockId + " is currently being imported");
            }
        } else {
            files = new HashSet<>();
        }
        files.add(lockId);
    }

    private static synchronized void finishImport(final Graph g, final String lockId, final boolean success) {
        Set<String> files = importsInProgress.get(g);
        if (null != files) {
            files.remove(lockId);
        }

        if (success) {
            files = importsSucceeded.computeIfAbsent(g, k -> new HashSet<>());
            files.add(lockId);
        }
    }

    private String findLockId() {
        return null != getFile() ? getFile().getAbsolutePath() : DEFAULT;
    }
}
