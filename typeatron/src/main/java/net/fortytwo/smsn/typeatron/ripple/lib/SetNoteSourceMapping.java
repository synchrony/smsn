package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.logging.Logger;

public class SetNoteSourceMapping extends NoteMapping {

    private static final Logger logger = Logger.getLogger(SetNoteSourceMapping.class.getName());

    public SetNoteSourceMapping(final BrainClient client,
                                final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "set-note-source"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{
                new Parameter("note", "the reference note", true),
                new Parameter("source", "the new data source", true)};
    }

    public String getComment() {
        return "sets the @source property of a note";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object value = stack.getFirst();
        stack = stack.getRest();
        Object no = stack.getFirst();
        stack = stack.getRest();

        TreeNode<Link> n = toTree(no, 0, false);

        if (null == n) {
            logger.warning("can't set @shortcut of non-note: " + no);
        } else {
            setProperty(n, SemanticSynchrony.PropertyKeys.SOURCE, "" + value);

            // put the note back on the stack
            solutions.accept(stack.push(n));
        }
    }
}
