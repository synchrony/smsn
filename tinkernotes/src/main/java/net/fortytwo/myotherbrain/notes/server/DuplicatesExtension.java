package net.fortytwo.myotherbrain.notes.server;

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
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.MOBGraph;
import net.fortytwo.myotherbrain.notes.Filter;
import net.fortytwo.ripple.StringUtils;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * A service for identifying atoms with duplicate values.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "duplicates")
public class DuplicatesExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for viewing TinkerNotes browsing history")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability) {
        logInfo("tinkernotes duplicates");

        Params p = new Params();
        p.baseGraph = graph;
        p.context = context;

        return handleRequestInternal(p, minWeight, maxWeight, minSharability, maxSharability);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        List<String> ids = getDuplicates(p.graph, p.filter);

        addView(p.semantics.customView(ids, p.filter), p);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean isReadOnly() {
        return true;
    }

    protected static final int MAX_DUPLICATES = 1000;

    private List<String> getDuplicates(final MOBGraph graph,
                                       final Filter filter) throws Exception {
        Map<String, List<String>> m = new HashMap<String, List<String>>();
        List<List<String>> dups = new LinkedList<List<String>>();
        int total = 0;

        for (Vertex v : graph.getGraph().getVertices()) {
            Atom a = graph.getAtom(v);

            if (filter.isVisible(a)) {
                String value = a.getValue();
                if (null != value && 0 < value.length()) {
                    String hash = StringUtils.md5SumOf(value);
                    List<String> ids = m.get(hash);
                    if (null == ids) {
                        ids = new LinkedList<String>();
                        m.put(hash, ids);
                    } else {
                        if (1 == ids.size()) {
                            total++;
                            dups.add(ids);
                        }

                        total++;
                        if (total > MAX_DUPLICATES) {
                            logInfo("showing only the first " + MAX_DUPLICATES + " duplicates");
                            break;
                        }
                    }

                    ids.add((String) v.getId());
                }
            }
        }

        List<String> allDups = new LinkedList<String>();
        for (List<String> l : dups) {
            allDups.addAll(l);
        }

        return allDups;
    }
}
