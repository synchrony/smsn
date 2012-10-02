package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import com.tinkerpop.blueprints.oupls.jung.GraphJung;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import edu.uci.ics.jung.algorithms.scoring.PageRank;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.MOBGraph;
import net.fortytwo.myotherbrain.MyOtherBrain;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.regex.Pattern;

/**
 * A service for exporting/serializing a TinkerNotes graph
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "export")
//@ExtensionDescriptor(description = "export/serialize a TinkerNotes graph")
public class ExportExtension extends TinkerNotesExtension {
    private static final Pattern DATE_PATTERN = Pattern.compile("[0-9]{4}-[0-9]{2}-[0-9]{2}");

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for exporting a MyOtherBrain graph for analysis in R")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph) {
        logInfo("tinkernotes export");

        // TODO: any security restrictions here?

        Params p = createParams(context, (KeyIndexableGraph) graph);

        return handleRequestInternal(p);
    }

    private void exportVertices(final MOBGraph g,
                                final PrintStream p) throws IOException {

        p.println("created\tid\tweight\tsharability\tvalue\talias");

        for (Vertex v : g.getGraph().getVertices()) {
            p.print(v.getProperty(MyOtherBrain.CREATED));
            p.print('\t');
            p.print(v.getId());
            p.print('\t');
            p.print(v.getProperty(MyOtherBrain.WEIGHT));
            p.print('\t');
            p.print(v.getProperty(MyOtherBrain.SHARABILITY));
            p.print('\t');

            p.print(escapeValue((String) v.getProperty(MyOtherBrain.VALUE)));
            p.print('\t');

            String alias = (String) v.getProperty(MyOtherBrain.ALIAS);
            if (null != alias) {
                p.print(escapeValue(alias));
            }

            p.print('\n');
        }
    }

    private void exportEdges(final MOBGraph g,
                             final PrintStream p) throws IOException {
        p.println("from\tto");
        for (Edge e : g.getGraph().getEdges()) {
            p.print(e.getVertex(Direction.OUT).getId());
            p.print('\t');
            p.print(e.getVertex(Direction.IN).getId());
            p.print('\n');
        }
    }

    private void exportTimeline(final MOBGraph g,
                                final PrintStream p) throws IOException {

        p.println("date\tid\tweight\tsharability\tvalue\talias");

        for (Vertex v : g.getGraph().getVertices()) {
            Atom d = g.getAtom(v);
            String date = d.getValue();

            if (DATE_PATTERN.matcher(date).matches()) {
                /* TODO
                for (Atom a : d.getInNotes()) {
                    p.print(date);
                    p.print('\t');
                    p.print(a.asVertex().getId());
                    p.print('\t');
                    p.print(a.getWeight());
                    p.print('\t');
                    p.print(a.getSharability());
                    p.print('\t');

                    p.print(escapeValue((String) v.getProperty(MyOtherBrain.VALUE)));
                    p.print('\t');

                    String alias = (String) v.getProperty(MyOtherBrain.ALIAS);
                    if (null != alias) {
                        p.print(escapeValue(alias));
                    }

                    p.print('\n');
                } */
            }
        }
    }

    private void exportPageRank(final MOBGraph g,
                                final PrintStream p) {
        TinkerGraph g2 = new TinkerGraph();
        for (Vertex v : g.getGraph().getVertices()) {
            g2.addVertex(v.getId());
        }
        for (Edge e : g.getGraph().getEdges()) {
            g2.addEdge(null,
                    g2.getVertex(e.getVertex(Direction.OUT).getId()),
                    g2.getVertex(e.getVertex(Direction.IN).getId()),
                    "link");
        }

        PageRank<Vertex, Edge> pr = new PageRank<Vertex, Edge>(new GraphJung(g2), 0.15d);
        pr.evaluate();

        p.println("id\tscore");
        for (Vertex v : g2.getVertices()) {
            p.println(v.getId() + "\t" + pr.getVertexScore(v));
        }

        g2.shutdown();
    }

    // Note: quote characters (") need to be replaced, e.g. with underscores (_), if this data is imported into R.
    // Otherwise, R becomes confused and skips rows.
    private String escapeValue(final String value) {
        return MyOtherBrain.unicodeEscape(value);
    }

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

        out = new FileOutputStream(new File("/tmp/tinkernotes-timeline.txt"));
        try {
            exportTimeline(p.graph, new PrintStream(out));
        } finally {
            out.close();
        }

        /* This takes a disproportionate amount of time.
        out = new FileOutputStream(new File("/tmp/tinkernotes-pagerank.txt"));
        try {
            exportPageRank(p.graph, new PrintStream(out));
        } finally {
            out.close();
        }//*/

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }
}
