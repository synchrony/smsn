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
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "export")
public class ExportExtension extends TinkerNotesExtension {
    private static final Pattern DATE_PATTERN = Pattern.compile("[0-9]{4}-[0-9]{2}-[0-9]{2}");

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for exporting a MyOtherBrain graph for analysis in R")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph) {
        logInfo("tinkernotes export");

        // TODO: any security restrictions here?

        Params p = createParams(context, graph);

        return handleRequestInternal(p);
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

    private void exportTimeline(final MOBGraph g,
                                final PrintStream p) throws IOException {

        p.println("date\tid\tweight\tsharability\tvalue");

        for (Vertex v : g.getGraph().getVertices()) {
            Atom d = g.getAtom(v);
            String date = d.getValue();

            if (DATE_PATTERN.matcher(date).matches()) {
                for (Atom a : d.getInNotes()) {
                    p.print(date);
                    p.print('\t');
                    p.print(a.asVertex().getId());
                    p.print('\t');
                    p.print(a.getWeight());
                    p.print('\t');
                    p.print(a.getSharability());
                    p.print('\t');

                    String value = a.getValue();
                    if (null != value) {
                        p.print(escapeValue(value));
                    }
                    p.print('\n');
                }
            }
        }
    }

    // Note: quote characters (") need to be replaced, e.g. with underscores (_), if this data is imported into R.
    // Otherwise, R becomes confused and skips rows.
    private String escapeValue(final String value) {
        return MyOtherBrain.unicodeEscape(value);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        p.graph.saveStats();

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

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }
}
