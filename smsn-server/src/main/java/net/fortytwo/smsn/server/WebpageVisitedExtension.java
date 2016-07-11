package net.fortytwo.smsn.server;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.impls.sail.SailGraph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.rdfagents.RDFAgents;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.IRI;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
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
@ExtensionNaming(namespace = "smsn", name = "webpage-visited")
public class WebpageVisitedExtension extends AbstractRexsterExtension {
    private static final String
            AAIR_NS = "http://xmlns.notu.be/aair#",
            DC_NS = "http://purl.org/dc/terms/",
            FOAF_NS = "http://xmlns.com/foaf/0.1/",
            EVENTS_NS = "http://fortytwo.net/2012/06/myotherbrain-events#", // temporary namespace
            RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    private static final IRI
            FOAF_DOCUMENT = RDFAgents.createIRI(FOAF_NS + "Document"),
            IDENTIFIER = RDFAgents.createIRI(DC_NS + "identifier"),
            RDF_TYPE = RDFAgents.createIRI(RDF_NS + "type"),
            WEBPAGE_VISIT = RDFAgents.createIRI(EVENTS_NS + "WebpageVisit"),
            PAGE = RDFAgents.createIRI(EVENTS_NS + "page"),
            TIME = RDFAgents.createIRI(EVENTS_NS + "time"),
            TITLE = RDFAgents.createIRI(DC_NS + "title");

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for logging web page visits")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "url",
                                                   description = "URL of the visited web page") String url,
                                           @ExtensionRequestParameter(name = "title",
                                                   description = "title of the visited web page") String title) {
        if (!(graph instanceof SailGraph)) {
            return ExtensionResponse.error("specified graph is not an RDF graph");
        }

        if (null == url || 0 == url.length()) {
            return ExtensionResponse.error("missing 'url' parameter");
        }

        Sail sail = ((SailGraph) graph).getRawGraph();
        DatasetFactory f = new DatasetFactory(sail.getValueFactory());
        ValueFactory vf = sail.getValueFactory();

        Collection<Statement> s = new LinkedList<>();
        IRI event = f.randomIRI();
        IRI page;
        try {
            page = vf.createIRI(url);
        } catch (Exception e) {
            return ExtensionResponse.error("'url' parameter is not a valid IRI: " + url);
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
