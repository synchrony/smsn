package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.ExtendoBrain;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.NoteQueries;
import net.fortytwo.extendo.typeatron.ripple.ExtendoBrainClient;
import net.fortytwo.extendo.util.TypedProperties;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AddToStreamMapping extends AtomMapping {
    private static final Logger logger = Logger.getLogger(AddToStreamMapping.class.getName());

    private final String brainStream;

    public AddToStreamMapping(final ExtendoBrainClient client,
                              final Filter filter) throws RippleException {
        super(client, filter);
        try {
            brainStream = Extendo.getConfiguration().getString(ExtendoBrain.PROP_BRAINSTREAM, null);
        } catch (TypedProperties.PropertyException e) {
            throw new RippleException(e);
        }
    }

    public String[] getIdentifiers() {
        return new String[]{
                ExtendoLibrary.NS_2014_12 + "add-to-stream"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{
                new Parameter("value", "the value of the atom to add", true),
        };
    }

    public String getComment() {
        return "adds a string as the @value of a new atom in a stream";
    }

    public void apply(RippleList stack,
                      Sink<RippleList> solutions,
                      ModelConnection mc) throws RippleException {
        if (null != brainStream) {
            String value = mc.toString(stack.getFirst());
            stack = stack.getRest();

            prepend(value);

            solutions.put(stack);
        } else {
            // TODO: send a warning cue to the Typeatron
            logger.warning("can't append to brain-stream; none has been configured");
        }
    }

    private void prepend(final String value) throws RippleException {
        Note note = new Note();
        note.setId(Extendo.createRandomKey());
        note.setSharability(filter.getDefaultSharability());
        note.setWeight(filter.getDefaultWeight());
        note.setCreated(System.currentTimeMillis());
        note.setValue(value);

        Note streamNote = new Note();
        streamNote.setId(brainStream);
        streamNote.addChild(note);

        try {
            client.update(streamNote, 1, filter, NoteQueries.forwardAddOnlyViewStyle);
        } catch (ExtendoBrainClient.ExtendoBrainClientException e) {
            throw new RippleException(e);
        }
    }
}
