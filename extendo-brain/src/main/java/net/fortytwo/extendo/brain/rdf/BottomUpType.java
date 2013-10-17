package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.BrainGraph;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.logging.Logger;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class BottomUpType {
    private static final Logger LOGGER = Extendo.getLogger(BottomUpType.class);

    public abstract Field[] getFields();

    /**
     * @return a built-in constraint on the value of instances of this type.
     *         It may be overridden in certain contexts.
     *         If null, there is no context-independent constraint.
     */
    public abstract Pattern getValueRegex();

    public abstract boolean additionalConstraintsSatisfied(String value);

    public abstract boolean aliasRequired();

    public abstract boolean childrenRequired();

    public abstract URI translateToRDF(Atom a,
                                        ValueFactory vf,
                                        RDFHandler handler) throws RDFHandlerException;

    protected URI translateTypeAndAlias(final Atom a,
                                        final ValueFactory vf,
                                        final RDFHandler handler,
                                        final URI type) throws RDFHandlerException {
        URI self = vf.createURI(BrainGraph.uriOf(a));

        handler.handleStatement(vf.createStatement(self, RDF.TYPE, type));

        if (null != a.getAlias()) {
            URI aliasURI;

            try {
                aliasURI = vf.createURI(a.getAlias());
            } catch (Exception e) {
                LOGGER.warning("alias is not a valid URI: " + a.getAlias());
                // TODO: temporary (to find the type of exception which is thrown)
                e.printStackTrace(System.err);
                aliasURI = null;
            }

            if (null != aliasURI) {
                handler.handleStatement(vf.createStatement(self, OWL.SAMEAS, vf.createURI(a.getAlias())));
            }
        }

        return self;
    }
}
