package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.MyOtherBrain;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Iterator;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "export")
public class ExportExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for exporting a MyOtherBrain graph for analysis in R")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "file", description = "the path of the file to which to export") String file) {
        LOGGER.info("export request for graph " + graph);

        if (null == file || file.length() == 0) {
            return ExtensionResponse.error("missing or empty 'file' parameter");
        }

        Params p = new Params();
        p.graph = graph;
        p.file = file;
        return this.handleRequestInternal(p);
    }

    private void exportToR(final Graph g,
                           final PrintStream p) throws IOException {
        for (Vertex v : g.getVertices()) {
            p.print(v.getProperty(MyOtherBrain.KEY));
            p.print('\t');
            Vertex from = getFrom(v);
            Vertex to = getTo(v);
            if (null != from) {
                p.print(from.getProperty(MyOtherBrain.KEY));
            }
            p.print('\t');
            if (null != to) {
                p.print(to.getProperty(MyOtherBrain.KEY));
            }
            p.print('\t');
            p.print(v.getProperty(MyOtherBrain.VALUE));
            p.println("");
        }
    }

    private Vertex getFrom(final Vertex v) {
        Iterator<Edge> edges = v.getOutEdges(MyOtherBrain.FROM).iterator();
        return edges.hasNext() ? edges.next().getInVertex() : null;
    }

    private Vertex getTo(final Vertex v) {
        Iterator<Edge> edges = v.getOutEdges(MyOtherBrain.TO).iterator();
        return edges.hasNext() ? edges.next().getInVertex() : null;
    }

    @Override
    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        OutputStream out = new FileOutputStream(new File(p.file));
        try {
            exportToR(p.graph, new PrintStream(out));
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
