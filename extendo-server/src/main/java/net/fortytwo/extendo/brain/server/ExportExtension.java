package net.fortytwo.extendo.brain.server;

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
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.AtomList;
import net.fortytwo.extendo.brain.BrainGraph;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.regex.Pattern;

/**
 * A service for exporting/serializing an Extend-o-Brain graph
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "export")
//@ExtensionDescriptor(description = "export/serialize a Extend-o-Brain graph")
public class ExportExtension extends ExtendoExtension {
    private static final Pattern DATE_PATTERN = Pattern.compile("[0-9]{4}-[0-9]{2}-[0-9]{2}");

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for exporting an Extend-o-Brain graph for analysis in tools such as R")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph) {
        logInfo("extendo export");

        // TODO: any security restrictions here?

        Params p = createParams(context, (KeyIndexableGraph) graph);

        return handleRequestInternal(p);
    }

    private void exportVertices(final BrainGraph g,
                                final PrintStream p) throws IOException {

        p.println("created\tid\tweight\tsharability\tvalue\talias");

        for (Vertex v : g.getGraph().getVertices()) {
            Object c = v.getProperty(Extendo.CREATED);
            if (null != c) {
                p.print(c);
                p.print('\t');
                p.print(v.getId());
                p.print('\t');
                p.print(v.getProperty(Extendo.WEIGHT));
                p.print('\t');
                p.print(v.getProperty(Extendo.SHARABILITY));
                p.print('\t');

                String value = v.getProperty(Extendo.VALUE);
                if (null == value) {
                    LOGGER.warning("note has null @value: " + v.getId());
                } else {
                    p.print(escapeValue((String) v.getProperty(Extendo.VALUE)));
                }
                p.print('\t');

                String alias = v.getProperty(Extendo.ALIAS);
                if (null != alias) {
                    p.print(escapeValue(alias));
                }

                p.print('\n');
            }
        }
    }

    private void exportEdges(final BrainGraph g,
                             final PrintStream p) throws IOException {
        p.println("from\tto");

        for (Vertex v : g.getGraph().getVertices()) {
            Atom a = g.getAtom(v);
            if (null != a) {
                AtomList l = a.getNotes();
                while (null != l) {
                    p.print(v.getId());
                    p.print('\t');
                    p.print(l.getFirst().asVertex().getId());
                    p.print('\n');
                    l = l.getRest();
                }
            }
        }
    }

    private void exportPageRank(final BrainGraph g,
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
        return Extendo.unicodeEscape(value);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        OutputStream out = new FileOutputStream(new File("/tmp/extendo-vertices.txt"));
        try {
            exportVertices(p.brain.getBrainGraph(), new PrintStream(out));
        } finally {
            out.close();
        }

        out = new FileOutputStream(new File("/tmp/extendo-edges.txt"));
        try {
            exportEdges(p.brain.getBrainGraph(), new PrintStream(out));
        } finally {
            out.close();
        }

        /* This takes a disproportionate amount of time.
        out = new FileOutputStream(new File("/tmp/extendo-pagerank.txt"));
        try {
            exportPageRank(p.graph, new PrintStream(out));
        } finally {
            out.close();
        }//*/

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service (data is only written to the file system)
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }
}
