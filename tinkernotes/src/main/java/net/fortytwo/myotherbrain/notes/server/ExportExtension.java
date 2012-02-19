package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.MOBGraph;
import net.fortytwo.myotherbrain.MyOtherBrain;

import javax.ws.rs.core.SecurityContext;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.security.Principal;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "export")
public class ExportExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for exporting a MyOtherBrain graph for analysis in R")
    public ExtensionResponse handleRequest(@RexsterContext SecurityContext security,
                                           @RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph) {
        LOGGER.info("tinkernotes export ");
        System.err.println("tinkernotes export ");

        Principal user = null == security ? null : security.getUserPrincipal();

        // TODO: any security restrictions here?

        Params p = new Params();
        p.baseGraph = graph;
        return this.handleRequestInternal(p);
    }

    private void exportVertices(final MOBGraph g,
                                final PrintStream p) throws IOException {

        p.println("created\tid\tweight\tsharability\tvalue");

        for (Vertex v : g.getGraph().getVertices()) {
            p.print(v.getProperty(MyOtherBrain.CREATED));
            p.print('\t');
            p.print(v.getId());
            p.print('\t');
            p.print(v.getProperty(MyOtherBrain.WEIGHT));
            p.print('\t');
            p.print(v.getProperty(MyOtherBrain.SHARABILITY));
            p.print('\t');

            String value = (String) v.getProperty(MyOtherBrain.VALUE);
            if (null != value) {
                p.print(escapeValue(value));
            }
            p.print('\n');
        }
    }

    private void exportEdges(final MOBGraph g,
                             final PrintStream p) throws IOException {
        p.println("from\tto");
        for (Edge e : g.getGraph().getEdges()) {
            p.print(e.getOutVertex().getId());
            p.print('\t');
            p.print(e.getInVertex().getId());
            p.print('\n');
        }
    }

    // Note: quote characters (") need to be replaced, e.g. with underscores (_), if this data is imported into R.
    // Otherwise, R becomes confused and skips rows.
    private String escapeValue(final String value) {
        return MyOtherBrain.unicodeEscape(value);
    }

    @Override
    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        OutputStream out = new FileOutputStream(new File("/tmp/tinkernotes-vertices.txt"));
        try {
            exportVertices(p.graph, new PrintStream(out));
        } finally {
            out.close();
        }

        out = new FileOutputStream(new File("/tmp/tinkernotes-edges.txt"));
        try {
            exportEdges(p.graph, new PrintStream(out));
        } finally {
            out.close();
        }

        return ExtensionResponse.ok(p.map);
    }

    @Override
    protected boolean isReadOnly() {
        return true;
    }
}
