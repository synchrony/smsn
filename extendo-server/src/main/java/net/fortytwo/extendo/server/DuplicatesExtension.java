package net.fortytwo.extendo.server;

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
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.model.Graph;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
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
                                           @ExtensionRequestParameter(name = "request", description = "request description (JSON object)") String request) {
        Params p = createParams(context, (KeyIndexableGraph) graph);
        FilteredResultsRequest r;
        try {
            r = new FilteredResultsRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.filter = r.getFilter();

        logInfo("extendo duplicates");

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

    private static final String UTF_8 = "UTF-8";

    private static final MessageDigest MD5_DIGEST;

    static {
        try {
            MD5_DIGEST = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private List<String> getDuplicates(final BrainGraph graph,
                                       final Filter filter) throws Exception {
        Map<String, List<String>> m = new HashMap<String, List<String>>();
        List<List<String>> dups = new LinkedList<List<String>>();
        int total = 0;

        for (Vertex v : graph.getPropertyGraph().getVertices()) {
            Atom a = graph.getAtom(v);

            if (filter.isVisible(a)) {
                String value = a.getValue();
                if (null != value && 0 < value.length()) {
                    String hash = md5SumOf(value);
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

    // copied from Ripple's StringUtils so as to avoid a dependency
    private static String md5SumOf(final String plaintext) throws UnsupportedEncodingException {
        synchronized (MD5_DIGEST) {
            MD5_DIGEST.update(plaintext.getBytes(UTF_8));
        }

        byte[] digest = MD5_DIGEST.digest();

        String coded = "";

        for (byte b : digest) {
            String hex = Integer.toHexString(b);

            if (hex.length() == 1) {
                hex = "0" + hex;
            }

            hex = hex.substring(hex.length() - 2);
            coded += hex;
        }

        return coded;
    }
}
