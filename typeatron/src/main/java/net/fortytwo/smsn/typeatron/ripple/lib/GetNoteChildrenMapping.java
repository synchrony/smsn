package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;

import java.util.List;
import java.util.logging.Logger;

public class GetNoteChildrenMapping extends NoteMapping {

    private static final Logger logger = Logger.getLogger(GetNoteChildrenMapping.class.getName());

    public GetNoteChildrenMapping(final BrainClient client,
                                  final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "get-note-children"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("note", "the reference note", true)};
    }

    public String getComment() {
        return "gets the list of children of an note";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        stack = stack.getRest();

        Note note = toTree(first, 1, true);

        if (null == note) {
            logger.warning("can't get children of non-note: " + first);
        } else {
            List<Note> children = Model.getChildrenAsList(note);
            if (null != children && 0 != children.size()) {
                RippleList cur = mc.list();
                for (int i = children.size() - 1; i >= 0; i--) {
                    cur = cur.push(children.get(i));
                }
                // put both the children and the (synced) note back on the stack
                solutions.accept(stack.push(note).push(cur));
            }
        }
    }
}
