package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import net.fortytwo.extendo.rdf.vocab.ExtendoVocab;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TODOTask extends AtomClass {

    public TODOTask() {
        super(
                "todo",
                Pattern.compile("TODO: .+"),
                null,
                null
                );
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    @Override
    public URI toRDF(final Atom a,RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        URI self = handleTypeAndAlias(a, vf, handler, ExtendoVocab.TODO);

        // assumes the prefix "TODO:"
        String d = a.getValue().substring(5).trim();
        handler.handleStatement(vf.createStatement(self, RDFS.COMMENT, vf.createLiteral(d)));

        return self;
    }
}
