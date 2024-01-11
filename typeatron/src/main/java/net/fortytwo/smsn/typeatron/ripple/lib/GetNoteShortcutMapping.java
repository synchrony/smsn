package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.logging.Logger;

public class GetNoteShortcutMapping extends NoteMapping {

    private static final Logger logger = Logger.getLogger(GetNoteShortcutMapping.class.getName());

    public GetNoteShortcutMapping(final BrainClient client,
                                  final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "get-note-shortcut"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("note", "the reference note", true)};
    }

    public String getComment() {
        return "gets the @shortcut property of a note";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        stack = stack.getRest();

        TreeNode<Link> n = toTree(first, 0, true);

        if (null == n) {
            logger.warning("can't get @shortcut of non-note: " + first);
        } else {
            String value = TreeViews.getShortcut(n);
            if (null != value) {
                // put both the @shortcut property and the (synced) note back on the stack
                solutions.accept(stack.push(n).push(value));
            }
        }
    }
}
