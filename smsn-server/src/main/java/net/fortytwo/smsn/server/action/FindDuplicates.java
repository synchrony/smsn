package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.FilteredResultsRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * A service for identifying atoms with duplicate values.
 */
public class FindDuplicates extends Action {

    @Override
    public String getName() {
        return "duplicates";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams params) throws JSONException {
        FilteredResultsRequest r;
        r = new FilteredResultsRequest(request, params.getUser());

        params.setFilter(r.getFilter());
    }

    protected void performTransaction(final RequestParams params) throws RequestProcessingException {
        List<String> ids = getDuplicates(params.getBrain().getAtomGraph(), params.getFilter());

        try {
            addView(params.getQueries().customView(ids, params.getFilter()), params);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }

    private static final int MAX_DUPLICATES = 1000;

    private static final String UTF_8 = "UTF-8";

    private static final MessageDigest MD5_DIGEST;

    static {
        try {
            MD5_DIGEST = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private List<String> getDuplicates(final AtomGraph graph,
                                       final Filter filter) throws RequestProcessingException {
        Map<String, List<String>> m = new HashMap<>();
        List<List<String>> dups = new LinkedList<>();
        int total = 0;

        for (Atom a : graph.getAllAtoms()) {
            if (filter.isVisible(a)) {
                String value = a.getValue();
                if (null != value && 0 < value.length()) {
                    String hash = null;
                    try {
                        hash = md5SumOf(value);
                    } catch (UnsupportedEncodingException e) {
                        throw new RequestProcessingException(e);
                    }
                    List<String> ids = m.get(hash);
                    if (null == ids) {
                        ids = new LinkedList<>();
                        m.put(hash, ids);
                    } else {
                        if (1 == ids.size()) {
                            total++;
                            dups.add(ids);
                        }

                        total++;
                        if (total > MAX_DUPLICATES) {
                            SemanticSynchrony.logInfo("showing only the first " + MAX_DUPLICATES + " duplicates");
                            break;
                        }
                    }

                    ids.add(a.getId());
                }
            }
        }

        List<String> allDups = new LinkedList<>();
        dups.forEach(allDups::addAll);

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
