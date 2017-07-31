package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;

import java.util.logging.Logger;

public class AddToStreamMapping extends NoteMapping {
    private static final Logger logger = Logger.getLogger(AddToStreamMapping.class.getName());

    private final String brainStream;

    public AddToStreamMapping(final BrainClient client,
                              final Filter filter) {
        super(client, filter);
        brainStream = SemanticSynchrony.getConfiguration().getBrainstream();
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "add-to-stream"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{
                new Parameter("value", "the value of the note to add", true),
        };
    }

    public String getComment() {
        return "adds a string as the @value of a new note in a stream";
    }

    public void apply(RippleList stack,
                      Sink<RippleList> solutions,
                      ModelConnection mc) throws RippleException {
        if (null != brainStream) {
            String value = mc.toString(stack.getFirst());
            stack = stack.getRest();

            prepend(value);

            solutions.accept(stack);
        } else {
            // TODO: send a warning cue to the Typeatron
            logger.warning("can't append to brain-stream; none has been configured");
        }
    }

    private void prepend(final String value) throws RippleException {
        TreeNode<Link> note = TreeNodeDTO.createEmptyNode();
        TreeViews.setId(note, SemanticSynchrony.createRandomId());
        TreeViews.setSource(note, filter.getDefaultSource());
        TreeViews.setWeight(note, filter.getDefaultWeight());
        TreeViews.setCreated(note, System.currentTimeMillis());
        TreeViews.setTitle(note, value);

        TreeNode<Link> streamTreeNode = TreeNodeDTO.createEmptyNode();
        TreeViews.setId(streamTreeNode, brainStream);
        streamTreeNode.addChild(note);

        try {
            client.update(streamTreeNode, 1, filter, ViewStyle.Basic.ForwardAddOnly.getStyle());
        } catch (BrainClient.BrainClientException e) {
            throw new RippleException(e);
        }
    }
}
