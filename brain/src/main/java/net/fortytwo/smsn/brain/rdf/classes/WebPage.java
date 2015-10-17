package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.rdf.AtomClass;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.rdf.vocab.FOAF;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.DCTERMS;
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
        handler.handleStatement(vf.createStatement(self, DCTERMS.TITLE, vf.createLiteral(d)));

        return self;
    }
}
