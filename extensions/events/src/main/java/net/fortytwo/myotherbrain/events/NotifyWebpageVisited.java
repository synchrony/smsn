package net.fortytwo.myotherbrain.events;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.impls.sail.SailGraph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "events", name = "notify-webpage-visited")
public class NotifyWebpageVisited extends AbstractRexsterExtension {
    private static final String
            AAIR_NS = "http://xmlns.notu.be/aair#",
            DC_NS = "http://purl.org/dc/terms/",
            FOAF_NS = "http://xmlns.com/foaf/0.1/",
            MOB_EVENTS_NS = "http://fortytwo.net/2012/06/myotherbrain-events#", // temporary namespace
            RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    private static final URI
            FOAF_DOCUMENT = new URIImpl(FOAF_NS + "Document"),
            IDENTIFIER = new URIImpl(DC_NS + "identifier"),
            RDF_TYPE = new URIImpl(RDF_NS + "type"),
            WEBPAGE_VISIT = new URIImpl(MOB_EVENTS_NS + "WebpageVisit"),
            PAGE = new URIImpl(MOB_EVENTS_NS + "page"),
            TIME = new URIImpl(MOB_EVENTS_NS + "time"),
            TITLE = new URIImpl(DC_NS + "title");

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for logging web page visits")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "url", description = "URL of the visited web page") String url,
                                           @ExtensionRequestParameter(name = "title", description = "title of the visited web page") String title) {
        if (!(graph instanceof SailGraph)) {
            return ExtensionResponse.error("specified graph is not an RDF graph");
        }

        if (null == url || 0 == url.length()) {
            return ExtensionResponse.error("missing 'url' parameter");
        }

        Sail sail = ((SailGraph) graph).getRawGraph();
        DatasetFactory f = new DatasetFactory(sail.getValueFactory());
        ValueFactory vf = sail.getValueFactory();

        Collection<Statement> s = new LinkedList<Statement>();
        URI event = f.randomURI();
        URI page;
        try {
            page = vf.createURI(url);
        } catch (Exception e) {
            return ExtensionResponse.error("'url' parameter is not a valid URI: " + url);
        }
        s.add(vf.createStatement(event, RDF_TYPE, WEBPAGE_VISIT));
        s.add(vf.createStatement(event, PAGE, page));

        long now = new Date().getTime();
        XMLGregorianCalendar c;
        try {
            c = toXMLGregorianCalendar(now);
        } catch (DatatypeConfigurationException e) {
            return ExtensionResponse.error(e);
        }
        s.add(vf.createStatement(event, TIME, vf.createLiteral(c)));

        if (null != title && 0 < title.length()) {
            s.add(vf.createStatement(event, TITLE, vf.createLiteral(title)));
        }

        Dataset d = new Dataset(s);

        // No need to *receive* this dataset, as it is asserted locally,
        // rather than received from another agent
        //AgentId agent = new AgentId(JOSH, JOSH_EMAIL);
        //d = f.receiveDataset(d, agent);

        try {
            SailConnection sc = sail.getConnection();
            try {
                for (Statement st : d.getStatements()) {
                    sc.addStatement(st.getSubject(), st.getPredicate(), st.getObject(), st.getContext());
                }

                sc.commit();
            } finally {
                sc.close();
            }
        } catch (SailException e) {
            return ExtensionResponse.error(e);
        }

        System.out.println("web page visited at " + now + ": " + url);

        return ExtensionResponse.ok(new HashMap());
    }

    private XMLGregorianCalendar toXMLGregorianCalendar(final long time) throws DatatypeConfigurationException {
        GregorianCalendar c = new GregorianCalendar();
        c.setTime(new Date(time));
        return DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
    }
}
