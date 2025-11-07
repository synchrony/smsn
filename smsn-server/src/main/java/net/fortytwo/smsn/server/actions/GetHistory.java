package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

/**
 * A service for finding recently visited notes
 */
public class GetHistory extends FilteredAction {

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException, BadRequestException {
        // History still uses old Note interface
        Iterable<Note> notes = getHistory(context.getBrain().getTopicGraph(), getFilter());

        // Convert to Atoms
        List<Atom> atoms = new ArrayList<>();
        for (Note note : notes) {
            atoms.add(context.getRepository().load(Note.getId(note)));
        }

        // Build list view
        TreeViewBuilder builder = new TreeViewBuilder(context.getRepository());
        net.fortytwo.smsn.brain.TreeNode tree = builder.buildListView(atoms, getFilter());

        try {
            JSONObject json = context.getTreeNodeJsonPrinter().toJson(tree);
            context.getMap().put(net.fortytwo.smsn.brain.Params.VIEW, json);
        } catch (java.io.IOException e) {
            throw new RequestProcessingException(e);
        }
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
