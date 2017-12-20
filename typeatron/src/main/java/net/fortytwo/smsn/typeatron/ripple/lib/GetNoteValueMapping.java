package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;

import java.util.logging.Logger;

public class GetNoteValueMapping extends NoteMapping {

    private static final Logger logger = Logger.getLogger(GetNoteValueMapping.class.getName());

    public GetNoteValueMapping(final BrainClient client,
                               final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "get-note-value"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("note", "the reference note", true)};
    }

    public String getComment() {
        return "gets the @value property of a note";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        stack = stack.getRest();

        Note n = toTree(first, 0, true);

        if (null == n) {
            logger.warning("can't get @value of non-note: " + first);
        } else {
            String value = n.getLabel();
            if (null == value) {
                logger.warning("note note has no @value: " + n);
            } else {
                // put both the @value property and the (synced) note back on the stack
                solutions.accept(stack.push(n).push(value));
            }
        }
    }
}
