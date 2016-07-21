package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.brain.Filter;
import net.fortytwo.smsn.brain.Note;
import net.fortytwo.smsn.brain.NoteQueries;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ShortcutSearchMapping extends AtomMapping {

    private static final Logger logger = Logger.getLogger(ShortcutSearchMapping.class.getName());

    public ShortcutSearchMapping(final BrainClient client,
                                 final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "shortcut-search"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("shortcut", "shortcut of the desired atom", true)};
    }

    public String getComment() {
        return "finds the atom with a given shortcut if one exists";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        String shortcut = mc.toString(stack.getFirst());
        stack = stack.getRest();

        List<Note> results = shortcutSearch(shortcut);
        for (Note n : results) {
            solutions.accept(stack.push(n));
        }
    }

    private List<Note> shortcutSearch(final String shortcut) throws RippleException {
        try {
            return client.search(NoteQueries.QueryType.Shortcut, shortcut, 1, filter, NoteQueries.forwardViewStyle);
        } catch (BrainClient.BrainClientException e) {
            throw new RippleException(e);
        }
    }
}
