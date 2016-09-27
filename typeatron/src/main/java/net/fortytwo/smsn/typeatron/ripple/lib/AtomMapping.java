package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.NoteQueries;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.wiki.NoteReader;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.SimpleValueFactory;

import java.util.logging.Logger;

public abstract class AtomMapping extends PrimitiveStackMapping {
    private static final Logger logger = Logger.getLogger(GetAtomValueMapping.class.getName());

    private static final ValueFactory valueFactory = SimpleValueFactory.getInstance();

    protected final BrainClient client;
    protected final Filter filter;

    protected AtomMapping(final BrainClient client,
                          final Filter filter) {
        this.client = client;
        this.filter = filter;
    }

    protected Note toNote(Object o, int height, boolean sync) throws RippleException {
        if (o instanceof String) {
            if (NoteReader.ID.matcher((String) o).matches()) {
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

    protected void setProperty(final Note n, final String name, final String value) throws RippleException {
        try {
            client.setProperty(n, name, value);
        } catch (BrainClient.BrainClientException e) {
            throw new RippleException(e);
        }
    }

    protected Float sharabilityOrWeightFromArgument(final Object arg, final ModelConnection mc)
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
                    throw new RippleException("illegal sharability or weight value: " + f);
                }
        }
    }

    protected IRI iriOf(final Note n) {
        String alias = n.getAlias();
        if (null != alias) {
            try {
                return valueFactory.createIRI(alias);
            } catch (IllegalArgumentException e) {
                logger.warning("alias " + alias + " is not an IRI");
            }
        }

        return valueFactory.createIRI(PGAtomGraph.iriForId(n.getId()));
    }
}
