package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.ontologies.Universe;
import net.fortytwo.smsn.rdf.RDFDataset;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.LinkedList;

/**
 * Base class for monitron sensor events.
 *
 * NOTE: Changed from rdfagents Dataset to RDFDataset in Dec 2024 during RDF4J migration.
 */
public abstract class MonitronEvent {
    private static final DatatypeFactory DATATYPE_FACTORY;

    static {
        try {
            DATATYPE_FACTORY = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    protected final ValueFactory valueFactory;
    protected final Collection<Statement> statements = new LinkedList<>();

    public MonitronEvent(final Context context) {
        this.valueFactory = context.getValueFactory();
    }

    public RDFDataset toRDF() {
        return new RDFDataset(statements);
    }

    protected IRI coinEventIRI() {
        return valueFactory.createIRI(Universe.NAMESPACE + "event-" + SemanticSynchrony.createRandomId().value);
    }

    protected Literal toLiteral(final Date d) {
        GregorianCalendar c = new GregorianCalendar();
        c.setTime(d);
        return valueFactory.createLiteral(DATATYPE_FACTORY.newXMLGregorianCalendar(c));
    }

    protected void addStatement(final Resource subject,
                                final IRI predicate,
                                final Value object) {
        statements.add(valueFactory.createStatement(subject, predicate, object));
    }
}
