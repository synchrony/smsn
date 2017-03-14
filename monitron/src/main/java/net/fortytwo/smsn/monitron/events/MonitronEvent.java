package net.fortytwo.smsn.monitron.events;

import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.ontologies.Universe;
import org.openrdf.model.IRI;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.LinkedList;

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

    public MonitronEvent(final Context context) {
        this.valueFactory = context.getValueFactory();
    }

    public Dataset toRDF() {
        return new Dataset(new LinkedList<>());
    }

    protected IRI coinEventIRI() {
        return valueFactory.createIRI(Universe.NAMESPACE + "event-" + SemanticSynchrony.createRandomId());
    }

    protected Literal toLiteral(final Date d) {
        GregorianCalendar c = new GregorianCalendar();
        c.setTime(d);
        return valueFactory.createLiteral(DATATYPE_FACTORY.newXMLGregorianCalendar(c));
    }

    protected void addStatement(final Dataset d,
                                final Resource subject,
                                final IRI predicate,
                                final Value object) {
        d.getStatements().add(valueFactory.createStatement(subject, predicate, object));
    }

}
