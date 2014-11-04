package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import net.fortytwo.extendo.rdf.vocab.DCTerms;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class WebPage extends AtomClass {

    public WebPage() {
        super(
                "webpage",
                Pattern.compile(".+ \\(web page\\)"),
                Pattern.compile("http(s)?://.+"),
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

        URI self = handleTypeAndAlias(a, vf, handler, FOAF.DOCUMENT);

        // note: assumes that the value ends with "(web page)"
        int i = a.getValue().lastIndexOf("(");
        String d = a.getValue().substring(0, i).trim();
        handler.handleStatement(vf.createStatement(self, DCTerms.TITLE, vf.createLiteral(d)));

        return self;
    }
}
