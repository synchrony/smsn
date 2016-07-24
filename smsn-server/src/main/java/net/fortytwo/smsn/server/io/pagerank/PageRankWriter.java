package net.fortytwo.smsn.server.io.pagerank;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import com.tinkerpop.blueprints.oupls.jung.GraphJung;
import edu.uci.ics.jung.algorithms.scoring.PageRank;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.server.io.BrainWriter;
import net.fortytwo.smsn.server.io.Format;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PageRankWriter extends BrainWriter {

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(PageRankTSVFormat.getInstance());
    }

    @Override
    protected void exportInternal(Brain sourceBrain, OutputStream destStream, Format format)
            throws IOException {
        AtomGraph sourceGraph = sourceBrain.getAtomGraph();

        TinkerGraph tmpGraph = new TinkerGraph();
        for (Vertex v : sourceGraph.getPropertyGraph().getVertices()) {
            tmpGraph.addVertex(v.getId());
        }
        for (Edge e : sourceGraph.getPropertyGraph().getEdges()) {
            tmpGraph.addEdge(null,
                    tmpGraph.getVertex(e.getVertex(Direction.OUT).getId()),
                    tmpGraph.getVertex(e.getVertex(Direction.IN).getId()),
                    "link");
        }

        PageRank<Vertex, Edge> pr = new PageRank<>(new GraphJung(tmpGraph), 0.15d);
        pr.evaluate();

        PrintStream p = new PrintStream(destStream);
        p.println("id\tscore");
        for (Vertex v : tmpGraph.getVertices()) {
            p.println(v.getId() + "\t" + pr.getVertexScore(v));
        }

        tmpGraph.shutdown();
    }
}
