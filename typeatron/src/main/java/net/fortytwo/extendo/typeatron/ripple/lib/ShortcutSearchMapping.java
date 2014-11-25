package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.NoteQueries;
import net.fortytwo.extendo.typeatron.ripple.ExtendoBrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import org.apache.http.HttpException;
import org.json.JSONException;

import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ShortcutSearchMapping extends AtomMapping {

    private static final Logger logger = Logger.getLogger(ShortcutSearchMapping.class.getName());

    public ShortcutSearchMapping(final ExtendoBrainClient client,
                                 final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                ExtendoLibrary.NS_2014_12 + "shortcut-search"
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
            solutions.put(stack.push(n));
        }
    }

    private List<Note> shortcutSearch(final String shortcut) throws RippleException {
        try {
            return client.search(NoteQueries.QueryType.Shortcut, shortcut, 1, filter, NoteQueries.FORWARD_ADJACENCY);
        } catch (JSONException e) {
            throw new RippleException(e);
        } catch (IOException e) {
            throw new RippleException(e);
        } catch (HttpException e) {
            throw new RippleException(e);
        }
    }
}
