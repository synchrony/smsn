package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomGraph;
import net.fortytwo.smsn.brain.rdf.classes.AKAReference;
import net.fortytwo.smsn.rdf.vocab.FOAF;
import org.openrdf.model.IRI;
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

    public AtomClass(final String name,
                     final Pattern valueRegex,
                     final Pattern aliasRegex,
                     final AtomRegex memberRegex) {
        this.name = name;
        this.valueRegex = valueRegex;
        this.aliasRegex = aliasRegex;
        this.memberRegex = memberRegex;
    }

    public String getName() {
        return name;
    }

    public Pattern getValueRegex() {
        return valueRegex;
    }

    public Pattern getAliasRegex() {
        return aliasRegex;
    }

    protected abstract boolean isCollectionClass();

    public abstract IRI toRDF(Atom a, RDFizationContext context) throws RDFHandlerException;

    protected IRI handleTypeAndAlias(final Atom a,
                                     final ValueFactory vf,
                                     final RDFHandler handler,
                                     final IRI type) throws RDFHandlerException {
        IRI self = vf.createIRI(AtomGraph.iriOf(a));

        handler.handleStatement(vf.createStatement(self, RDF.TYPE, type));

        if (null != a.getAlias()) {
            IRI aliasIRI;

            try {
                aliasIRI = vf.createIRI(a.getAlias());
            } catch (Exception e) {
                SemanticSynchrony.logWarning("alias is not a valid IRI: " + a.getAlias(), e);
                aliasIRI = null;
            }

            if (null != aliasIRI) {
                handler.handleStatement(vf.createStatement(self, OWL.SAMEAS, vf.createIRI(a.getAlias())));
            }
        }

        return self;
    }

    public interface FieldHandler {
        void handle(Atom object, RDFizationContext context) throws RDFHandlerException;
    }

    public static class NickHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();

            // TODO: this is an abuse of foaf:nick even when the domain is foaf:Person as it is here...
            // foaf:nick is supposed to be used for online handles, not aliases in general
            context.getHandler().handleStatement(
                    vf.createStatement(
                            context.getSubjectIri(), FOAF.NICK, vf.createLiteral(
                                    AKAReference.extractAlias(object.getValue()))));
        }
    }

    public static class PageHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            IRI objectIRI = context.iriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    // note: use of foaf:page rather than foaf:homepage avoids the assumption that the link is
                    // always a home page, although this is frequently the case
                    context.getSubjectIri(), FOAF.PAGE, objectIRI));
        }
    }

    public static class RFIDHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            // TODO

            // note: there are no relevant properties in the BTC 2014 matching "rfid":
            // grep -i rfid btc-predicates-frequency.tsv
            // 1       http://dbpedia-live.openlinksw.com/property/perfid
        }
    }

    public static class DocumentsAboutTopicHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            IRI objectIRI = context.iriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    objectIRI, FOAF.TOPIC, context.getSubjectIri()));
            // The skos:note on dc:subject reads "This term is intended to be used with non-literal values
            // as defined in the DCMI Abstract Model (http://dublincore.org/documents/abstract-model/) [...]"
            //objectIRI, DCTerms.SUBJECT, context.getSubjectIri()));
        }
    }
}
