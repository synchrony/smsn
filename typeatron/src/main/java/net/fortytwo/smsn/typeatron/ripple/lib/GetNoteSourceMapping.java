package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;

import java.util.logging.Logger;

public class GetNoteSourceMapping extends NoteMapping {

    private static final Logger logger = Logger.getLogger(GetNoteSourceMapping.class.getName());

    public GetNoteSourceMapping(final BrainClient client,
                                final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "get-note-source"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("note", "the reference note", true)};
    }

    public String getComment() {
        return "gets the @source property of a note";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        stack = stack.getRest();

        Note n = toTree(first, 0, true);

        if (null == n) {
            logger.warning("can't get @source of non-note: " + first);
        } else {
            String value = n.getSource();
            if (null != value) {
                // put both the @source property and the (synced) note back on the stack
                solutions.accept(stack.push(n).push(value));
            }
        }
    }
}
