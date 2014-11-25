package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.typeatron.ripple.ExtendoBrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GetAtomShortcutMapping extends AtomMapping {

    private static final Logger logger = Logger.getLogger(GetAtomShortcutMapping.class.getName());

    public GetAtomShortcutMapping(final ExtendoBrainClient client,
                                  final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                ExtendoLibrary.NS_2014_12 + "get-atom-shortcut"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("atom", "the reference atom", true)};
    }

    public String getComment() {
        return "gets the shortcut attribute of an atom";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        stack = stack.getRest();

        Note n = toNote(first, true);

        if (null == n) {
            logger.warning("can't get @shortcut of non-atom: " + first);
        } else {
            String value = n.getShortcut();
            if (null != value) {
                // put both the @shortcut and the (synced) atom back on the stack
                solutions.put(stack.push(n).push(value));
            }
        }
    }
}
