package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service to adjust a graph to data model changes
 */
public class ActionDuJour extends Action {
    @Override
    public void parseRequest(RequestParams params) throws IOException, BadRequestException {
        // nothing to do
    }

    @Override
    protected void performTransaction(RequestParams params) throws BadRequestException, RequestProcessingException {
        AtomGraph graph = params.getBrain().getAtomGraph();
        checkForProblematicMultiLineAtoms(graph);
    }

    private void checkForProblematicMultiLineAtoms(final AtomGraph graph) {
        for (Atom atom : graph.getAllAtoms()) {
            if (isProblematicMultiLineAtom(atom)) {
                System.out.println("atom cannot be migrated: " + atom.getId() + " (" + getValue(atom) + ")");
            }
        }
    }

    private boolean isProblematicMultiLineAtom(final Atom atom) {
        return countValueLines(atom) > 1 && (null != atom.getNotes());
    }

    private int countValueLines(final Atom atom) {
        String value = atom.getValue();
        if (null == value) {
            logger.warning("atom with null value: " + atom.getId());
            return 0;
        }
        String[] a = value.split("\\n");
        return a.length;
    }

    private String getValue(final Atom atom) {
        String value = atom.getValue();
        return value.length() > 30 ? value.substring(0,25) + "[...]" : value;
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
