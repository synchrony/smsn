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
        if (!hasProblematicMultiLineAtoms(graph)) {
            migrateAtoms(graph);
        }
    }

    private void migrateAtoms(final AtomGraph graph) {
        int countMigrated = 0;
        for (Atom atom : graph.getAllAtoms()) {
            if (countValueLines(atom) > 1) {
                atom.setPage(atom.getValue());
                atom.setTitle("RenameMe");
                countMigrated++;
            } else if (null != atom.getValue()) {
                atom.setTitle(atom.getValue());
            }
            atom.setValue(null);
        }
        logger.info("migrated " + countMigrated + " topic nodes");
    }

    private boolean hasProblematicMultiLineAtoms(final AtomGraph graph) {
        boolean problems = false;
        for (Atom atom : graph.getAllAtoms()) {
            if (isProblematicMultiLineAtom(atom)) {
                System.out.println("atom cannot be migrated: " + atom.getId() + " (" + getValue(atom) + ")");
                problems = true;
            }
        }
        return problems;
    }

    private boolean isProblematicMultiLineAtom(final Atom atom) {
        return countTitleLines(atom) > 1 || countValueLines(atom) > 1 && (null != atom.getNotes());
    }

    private int countTitleLines(final Atom atom) {
        String title = atom.getTitle();
        if (null == title) {
            //logger.warning("atom with null title: " + atom.getId());
            return 0;
        }
        String[] a = title.split("\\n");
        return a.length;
    }

    private int countValueLines(final Atom atom) {
        String value = atom.getValue();
        if (null == value) {
            //logger.warning("atom with null value: " + atom.getId());
            return 0;
        }
        String[] a = value.split("\\n");
        return a.length;
    }

    private String getValue(final Atom atom) {
        String value = atom.getTitle();
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
