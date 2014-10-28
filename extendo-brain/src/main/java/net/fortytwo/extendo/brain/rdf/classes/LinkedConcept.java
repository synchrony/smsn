package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class LinkedConcept extends AtomClass {
    public static final LinkedConcept INSTANCE = new LinkedConcept();

    public LinkedConcept() {
        super(
                "linked-concept",
                null,
                Pattern.compile("http://dbpedia.org/resource/.+"),
                null
                );
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    @Override
    public URI toRDF(Atom a, RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        URI self = handleTypeAndAlias(a, vf, handler, OWL.THING);

        // note: we assume short, name-like values for linked atoms
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getValue())));

        return self;
    }
}
