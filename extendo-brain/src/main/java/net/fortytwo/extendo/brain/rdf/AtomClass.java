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

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class AtomClass {

    protected String name;
    protected Pattern valueRegex;
    protected Pattern aliasRegex;
    protected AtomRegex memberRegex;
    private final int highestOutScore;

    public AtomClass(final String name,
                     final Pattern valueRegex,
                     final Pattern aliasRegex,
                     final AtomRegex memberRegex) {
        this.name = name;
        this.valueRegex = valueRegex;
        this.aliasRegex = aliasRegex;
        this.memberRegex = memberRegex;

        int tot = 0;
        if (null != valueRegex) {
            tot++;
        }
        if (null != aliasRegex) {
            tot++;
        }
        if (null != memberRegex) {
            for (AtomRegex.El el : memberRegex.getElements()) {
                tot += el.getAlternatives().size() > 0 ? 3 : 1;
            }
        }
        highestOutScore = tot;
    }

    public int getHighestOutScore() {
        return highestOutScore;
    }

    protected abstract boolean isCollectionClass();

    public abstract URI toRDF(Atom a,
                              RDFizationContext context) throws RDFHandlerException;

    protected URI handleTypeAndAlias(final Atom a,
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
                Extendo.logWarning("alias is not a valid URI: " + a.getAlias(), e);
                aliasURI = null;
            }

            if (null != aliasURI) {
                handler.handleStatement(vf.createStatement(self, OWL.SAMEAS, vf.createURI(a.getAlias())));
            }
        }

        return self;
    }

    public interface FieldHandler {
        void handle(Atom object, RDFizationContext context) throws RDFHandlerException;
    }
}
