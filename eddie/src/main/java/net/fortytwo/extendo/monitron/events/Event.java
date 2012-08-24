package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.MonitronEventHandler;
import net.fortytwo.extendo.ontologies.Universe;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.LinkedList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Event {
    private static final DatatypeFactory DATATYPE_FACTORY;

    static {
        try {
            DATATYPE_FACTORY = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    protected final MonitronEventHandler context;
    protected final ValueFactory vf;
    protected final Dataset d;

    public Event(final MonitronEventHandler context) {
        this.context = context;
        this.vf = context.getValueFactory();
        d = new Dataset(new LinkedList<Statement>());
    }

    public Dataset getDataset() {
        return d;
    }

    protected URI coinEventURI() {
        return vf.createURI(Universe.NAMESPACE + "event-" + MyOtherBrain.createRandomKey());
    }

    protected Literal toLiteral(final Date d) {
        GregorianCalendar c = new GregorianCalendar();
        c.setTime(d);
        return vf.createLiteral(DATATYPE_FACTORY.newXMLGregorianCalendar(c));
    }

    protected void addStatement(final Dataset d,
                              final Resource subject,
                              final URI predicate,
                              final Value object) {
        d.getStatements().add(vf.createStatement(subject, predicate, object));
    }

}
