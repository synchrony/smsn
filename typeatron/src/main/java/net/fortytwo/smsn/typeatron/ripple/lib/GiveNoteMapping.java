package net.fortytwo.smsn.typeatron.ripple.lib;

import com.illposed.osc.OSCMessage;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.rdf.vocab.SmSnActivityOntology;
import net.fortytwo.smsn.typeatron.TypeatronControl;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import org.openrdf.model.IRI;

import java.util.logging.Level;
import java.util.logging.Logger;

public class GiveNoteMapping extends NoteMapping {

    private static final Logger logger = Logger.getLogger(GiveNoteMapping.class.getName());
    private final TypeatronControl typeatron;

    public GiveNoteMapping(final BrainClient client,
                           final Filter filter,
                           final TypeatronControl typeatron) {
        super(client, filter);
        this.typeatron = typeatron;
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "give-note"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("note", "the reference note", true)};
    }

    public String getComment() {
        return "prepares a note for the 'give' half of a hand-off gesture";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        Note n = toTree(first, 0, true);

        if (null == n) {
            logger.warning("can't give non-note: " + first);
        } else {
            IRI iri = iriOf(n);

            // value is informational; it is used only for development/debugging purposes
            String value = n.getLabel();

            logger.log(Level.INFO, "preparing to give " + iri + " (" + value + ")");

            OSCMessage m = new OSCMessage(SmSnActivityOntology.EXO_ACTIVITY_GIVE);
            m.addArgument(typeatron.getAgent().getAgentIri().stringValue());
            m.addArgument(iri.stringValue());
            //m.addArgument(value);
            typeatron.getAgent().sendOSCMessageToCoordinator(m);

            // keep the stack unchanged
            solutions.accept(stack);
        }
    }
}
