package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import net.fortytwo.smsn.brain.AtomId;

import java.util.logging.Logger;

public abstract class NoteMapping extends PrimitiveStackMapping {
    private static final Logger logger = Logger.getLogger(GetNoteValueMapping.class.getName());

    private static final ValueFactory valueFactory = SimpleValueFactory.getInstance();

    protected final BrainClient client;
    protected final Filter filter;

    protected NoteMapping(final BrainClient client,
                          final Filter filter) {
        this.client = client;
        this.filter = filter;
    }

    protected TreeNode<Link> toTree(Object o, int height, boolean sync) throws RippleException {
        if (o instanceof String) {
            if (SemanticSynchrony.isValidId((String) o)) {
                TreeNode<Link> n = TreeNodeDTO.createEmptyNode();
                TreeViews.setId(n, new AtomId((String) o));
                o = n;
            } else {
                return null;
            }
        }

        if (o instanceof TreeNode) {
            TreeNode<Link> n = (TreeNode<Link>) o;
            if (null != TreeViews.getTitle(n) && !sync) {
                return n;
            } else {
                if (null == TreeViews.getId(n)) {
                    logger.warning("note with null id");
                    return null;
                } else if (sync) {
                    try {
                        n = client.view(n, height, filter, ViewStyle.Basic.Forward.getStyle(), false);
                    } catch (BrainClient.BrainClientException e) {
                        throw new RippleException(e);
                    }
                }

                return n;
            }
        } else {
            return null;
        }
    }

    protected void setProperty(final TreeNode<Link> n, final String name, final String value) throws RippleException {
        try {
            client.setProperty(n, name, value);
        } catch (BrainClient.BrainClientException e) {
            throw new RippleException(e);
        }
    }

    protected Float weightFromArgument(final Object arg, final ModelConnection mc)
            throws RippleException {

        String asString = mc.toString(arg);
        switch (asString) {
            case "a":
                return 0.25f;
            case "s":
                return 0.5f;
            case "d":
                return 0.75f;
            case "f":
                return 1.0f;
            default:
                Number n = mc.toNumber(arg);
                Float f = n.floatValue();
                if (f == 0.25f || f == 0.5f || f == 0.75f || f == 1.0f) {
                    return f;
                } else {
                    throw new RippleException("illegal weight value: " + f);
                }
        }
    }

    protected IRI iriOf(final TreeNode<Link> n) {
        String alias = TreeViews.getAlias(n);
        if (null != alias) {
            try {
                return valueFactory.createIRI(alias);
            } catch (IllegalArgumentException e) {
                logger.warning("alias " + alias + " is not an IRI");
            }
        }

        return valueFactory.createIRI("urn:smsn:" + TreeViews.getId(n).value);
    }
}
