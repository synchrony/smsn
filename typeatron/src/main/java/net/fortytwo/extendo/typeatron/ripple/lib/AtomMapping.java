package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.NoteQueries;
import net.fortytwo.extendo.brain.wiki.NoteParser;
import net.fortytwo.extendo.typeatron.ripple.ExtendoBrainClient;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class AtomMapping extends PrimitiveStackMapping {
    private static final Logger logger = Logger.getLogger(GetAtomValueMapping.class.getName());

    protected final ExtendoBrainClient client;
    protected final Filter filter;

    protected AtomMapping(final ExtendoBrainClient client,
                          final Filter filter) {
        this.client = client;
        this.filter = filter;
    }

    protected Note toNote(Object o, int height, boolean sync) throws RippleException {
        if (o instanceof String) {
            if (NoteParser.ID.matcher((String) o).matches()) {
                Note n = new Note();
                n.setId((String) o);
                o = n;
            } else {
                return null;
            }
        }

        if (o instanceof Note) {
            Note n = (Note) o;
            if (null != n.getValue() && !sync) {
                return n;
            } else {
                if (null == n.getId()) {
                    logger.warning("note with null id");
                    return null;
                } else if (sync) {
                    try {
                        n = client.view(n, height, filter, NoteQueries.forwardViewStyle, false);
                    } catch (ExtendoBrainClient.ExtendoBrainClientException e) {
                        throw new RippleException(e);
                    }
                }

                return n;
            }
        } else {
            return null;
        }
    }

    protected void setProperty(final Note n, final String name, final String value) throws RippleException {
        try {
            client.setProperty(n, name, value);
        } catch (ExtendoBrainClient.ExtendoBrainClientException e) {
            throw new RippleException(e);
        }
    }

    protected Float sharabilityOrWeightFromArgument(final Object arg, final ModelConnection mc)
            throws RippleException {

        String asString = mc.toString(arg);
        if (asString.equals("a")) {
            return 0.25f;
        } else if (asString.equals("s")) {
            return 0.5f;
        } else if (asString.equals("d")) {
            return 0.75f;
        } else if (asString.equals("f")) {
            return 1.0f;
        } else {
            Number n = mc.toNumber(arg);
            Float f = n.floatValue();
            if (f == 0.25f || f == 0.5f || f == 0.75f || f == 1.0f) {
                return f;
            } else {
                throw new RippleException("illegal sharability or weight value: " + f);
            }
        }
    }

    protected URI uriOf(final Note n) {
        String alias = n.getAlias();
        if (null != alias) {
            try {
                return new URIImpl(alias);
            } catch (IllegalArgumentException e) {
                logger.warning("alias " + alias + " is not a URI");
            }
        }

        return new URIImpl(BrainGraph.uriForId(n.getId()));
    }
}
