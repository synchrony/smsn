package net.fortytwo.smsn.server;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.TransactionalGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import com.tinkerpop.blueprints.oupls.jung.GraphJung;
import com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import edu.uci.ics.jung.algorithms.scoring.PageRank;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomList;
import net.fortytwo.smsn.brain.BrainGraph;
import net.fortytwo.smsn.brain.ExtendoBrain;
import net.fortytwo.smsn.brain.Filter;
import net.fortytwo.smsn.brain.NoteQueries;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.sail.SailException;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.security.Principal;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;

/**
 * A service for exporting an Extend-o-Brain graph to the file system
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "export")
public class ExportExtension extends SmSnExtension {

    private static final int MAX_LATEX_RECURSE_LEVELS = 16;

    private static final Set<String>
            sectionKeywords,
            nonbreakingKeywords;

    static {
        sectionKeywords = keywords("section", "subsection", "subsubsection");
        nonbreakingKeywords = keywords("begin", "caption", "end", "item", "label", "textbf");
    }

    private static Set<String> keywords(final String... labels) {
        Set<String> set = new HashSet<>();
        for (String l : labels) {
            set.add("\\" + l);
        }

        return set;
    }

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for exporting an Extend-o-Brain graph to the file system")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request)
            throws Exception {

        // TODO: any security restrictions here?
        try {
            RequestParams p = createParams(context, (KeyIndexableGraph) graph);

            ExportRequest r;
            try {
                r = new ExportRequest(new JSONObject(request), p.user);
            } catch (JSONException e) {
                return ExtensionResponse.error(e.getMessage());
            }

            p.filter = r.getFilter();
            p.file = r.file;
            p.format = r.format;

            p.rootId = r.rootId;
            p.height = r.height;

            SemanticSynchrony.logInfo("SmSn export " + r.format + " to " + r.file);

            return handleRequestInternal(p);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "export failed", e);
            throw e;
        }
    }

    private void exportVertices(final BrainGraph g,
                                final KnowledgeBase kb,
                                final PrintStream p) throws IOException {

        p.println("created\tid\tweight\tsharability\tclass\tout\tin\tvalue\talias");

        for (Vertex v : g.getPropertyGraph().getVertices()) {
            Object c = v.getProperty(SemanticSynchrony.CREATED);
            if (null != c) {
                p.print(c);
                p.print('\t');
                p.print(v.getId());
                p.print('\t');
                p.print((double) v.getProperty(SemanticSynchrony.WEIGHT));
                p.print('\t');
                p.print((double) v.getProperty(SemanticSynchrony.SHARABILITY));
                p.print('\t');

                List<KnowledgeBase.AtomClassEntry> entries = kb.getClassInfo(g.getAtom(v));
                if (null != entries && entries.size() > 0) {
                    KnowledgeBase.AtomClassEntry e = entries.get(0);
                    p.print(e.getInferredClassName());
                    p.print('\t');
                    p.print(e.getOutScore());
                    p.print('\t');
                    p.print(e.getInScore());
                    p.print('\t');
                } else {
                    p.print("\t0\t0\t");
                }

                String value = v.getProperty(SemanticSynchrony.VALUE);
                if (null == value) {
                    logger.warning("note has null @value: " + v.getId());
                } else {
                    p.print(escapeValue((String) v.getProperty(SemanticSynchrony.VALUE)));
                }
                p.print('\t');

                String alias = v.getProperty(SemanticSynchrony.ALIAS);
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

        for (Vertex v : g.getPropertyGraph().getVertices()) {
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
        for (Vertex v : g.getPropertyGraph().getVertices()) {
            g2.addVertex(v.getId());
        }
        for (Edge e : g.getPropertyGraph().getEdges()) {
            g2.addEdge(null,
                    g2.getVertex(e.getVertex(Direction.OUT).getId()),
                    g2.getVertex(e.getVertex(Direction.IN).getId()),
                    "link");
        }

        PageRank<Vertex, Edge> pr = new PageRank<>(new GraphJung(g2), 0.15d);
        pr.evaluate();

        p.println("id\tscore");
        for (Vertex v : g2.getVertices()) {
            p.println(v.getId() + "\t" + pr.getVertexScore(v));
        }

        g2.shutdown();
    }

    private void exportGraphML(final BrainGraph g,
                               final OutputStream out) throws IOException {
        ((TransactionalGraph) g.getPropertyGraph()).commit();
        GraphMLWriter w = new GraphMLWriter(g.getPropertyGraph());
        w.setNormalize(true);
        w.outputGraph(out);
    }

    private void exportRDF(final KnowledgeBase kb,
                           final RDFFormat format,
                           final Filter filter,
                           final OutputStream out) throws IOException {
        try {
            kb.exportRDF(out, format, filter);
        } catch (SailException | RDFHandlerException e) {
            throw new IOException(e);
        }
        out.flush();
        out.close();
    }

    private void writeLatex(final Atom root,
                            final Filter filter,
                            final int level,
                            final int sectionLevel,
                            final OutputStream out) throws IOException {

        if (!filter.isVisible(root.asVertex())) {
            return;
        }

        if (level >= MAX_LATEX_RECURSE_LEVELS) {
            logger.warning("LaTeX tree exceeds maximum depth of " + MAX_LATEX_RECURSE_LEVELS);
            return;
        }

        boolean isSec = false;
        boolean doRecurse = false;

        // trim immediately; don't try to preserve indentation or trailing whitespace
        String value = root.getValue().trim();
        String textOut;

        if (value.startsWith("\"")) {
            textOut = value.substring(1, value.endsWith("\"") ? value.length() - 1 : value.length());
        } else if (value.contains("\\n")) {
            // write verbatim blocks out verbatim
            // note: we don't expect any children of verbatim blocks
            textOut = value;
        } else if (value.startsWith("%")) {
            // Add an extra newline before demarcated paragraphs.
            // This saves on explicit line breaks in the source notes.
            textOut = "\n" + value;
            doRecurse = true;
        } else if (value.startsWith("\\")) {
            // Automatically correct section/subsection/subsubsection keywords according to the hierarchy;
            // this allows more flexibility w.r.t. including content trees in multiple documents.
            for (String keyword : sectionKeywords) {
                if (value.startsWith(keyword)) {
                    String replacement = sectionLevel > 1
                            ? "\\subsubsection" : sectionLevel > 0
                            ? "\\subsection" : "\\section";
                    value = replacement + value.substring(keyword.length());
                    isSec = true;
                    break;
                }
            }

            // for a few keywords, like /item, we don't need the extra space
            boolean doBreak = true;
            if (!isSec) {
                for (String keyword : nonbreakingKeywords) {
                    if (value.startsWith(keyword)) {
                        doBreak = false;
                        break;
                    }
                }
            }

            // also add an extra newline before Exobrain items which are
            // specifically LaTeX, e.g. chapters, sections, subsections, begin blocks.
            textOut = doBreak ? "\n" + value : value;
            doRecurse = true;
        } else {
            // anything else is ignored along with any children
            textOut = null;
        }

        if (null != textOut) {
            out.write(textOut.getBytes());
            out.write('\n');
        }

        if (doRecurse) {
            for (Atom child : NoteQueries.forwardViewStyle.getLinked(root, filter)) {
                writeLatex(child, filter, level + 1, isSec ? sectionLevel + 1 : sectionLevel, out);
            }
        }
    }

    private void exportLatexTree(final ExtendoBrain brain,
                                 final String root,
                                 final Filter filter,
                                 final OutputStream out) throws IOException {

        Atom rootAtom = brain.getBrainGraph().getAtom(root);
        if (null == rootAtom) {
            throw new IllegalStateException("no such atom: " + root);
        }

        writeLatex(rootAtom, filter, 0, 0, out);

        out.close();
    }

    // Note: quote characters (") need to be replaced, e.g. with underscores (_), if this data is imported into R.
    // Otherwise, R becomes confused and skips rows.
    private String escapeValue(final String value) {
        return SemanticSynchrony.unicodeEscape(value);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        Params.Format format = Params.Format.valueOf(p.format);
        if (null == format) {
            return ExtensionResponse.error("no such format: " + p.format);
        }

        Filter filter = null;
        try (OutputStream out = new FileOutputStream(p.file)) {
            switch (format) {
                case Vertices:
                    exportVertices(p.brain.getBrainGraph(), p.brain.getKnowledgeBase(), new PrintStream(out));
                    break;
                case Edges:
                    exportEdges(p.brain.getBrainGraph(), new PrintStream(out));
                    break;
                case GraphML:
                    exportGraphML(p.brain.getBrainGraph(), out);
                    break;
                case LaTeX:
                    exportLatexTree(p.brain, p.rootId, p.filter, out);
                    break;
                case PageRank:
                    exportPageRank(p.brain.getBrainGraph(), new PrintStream(out));
                    break;
                case Web:
                    filter = new Filter(0f, 1f, 0.5f, 0.5f, 1f, 0.75f);
                    // fall through
                case RDF:
                    RDFFormat rdfFormat = RDFFormat.forFileName(p.file);
                    if (null == rdfFormat) {
                        throw new IllegalStateException("no RDF format for file name: " + p.file);
                    }
                    exportRDF(p.brain.getKnowledgeBase(), rdfFormat, filter, out);
                    break;
                default:
                    throw new IllegalStateException();
            }
        }

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service (data is only written to the file system)
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }

    private class ExportRequest extends FilteredResultsRequest {
        private final String format;
        private final String file;

        private final String rootId;
        private final int height;

        public ExportRequest(final JSONObject json,
                             final Principal user) throws JSONException {
            super(json, user);

            format = this.json.getString(Params.FORMAT);
            file = this.json.getString(Params.FILE);

            rootId = this.json.optString(Params.ROOT);
            height = this.json.optInt(Params.HEIGHT, 0);
        }
    }
}
