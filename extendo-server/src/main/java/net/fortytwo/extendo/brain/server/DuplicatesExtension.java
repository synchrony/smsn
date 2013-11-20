package net.fortytwo.extendo.brain.server;

import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.ripple.StringUtils;
import org.openrdf.model.Graph;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * A service for identifying atoms with duplicate values.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "duplicates")
//@ExtensionDescriptor(description = "identify atoms with duplicate values")
public class DuplicatesExtension extends ExtendoExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for viewing Extend-o-Brain browsing history")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability) {
        logInfo("extendo duplicates");

        Params p = createParams(context, (KeyIndexableGraph) graph);
        p.filter = createFilter(p.user, minWeight, maxWeight, -1, minSharability, maxSharability, -1);

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        List<String> ids = getDuplicates(p.brain.getBrainGraph(), p.filter);

        addView(p.queries.customView(ids, p.filter), p);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }

    protected static final int MAX_DUPLICATES = 1000;

    private List<String> getDuplicates(final BrainGraph graph,
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
