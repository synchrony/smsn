package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.FilteredResultsRequest;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

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
public class FindDuplicates extends Action<FilteredResultsRequest> {

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

    @Override
    public String getName() {
        return "duplicates";
    }

    @Override
    public void parseRequest(final FilteredResultsRequest request, final RequestParams params) throws IOException {
        params.setFilter(request.getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException {
        List<List<Atom>> dups = getDuplicates(params.getBrain().getAtomGraph(), params.getFilter());
        List<Atom> flat = new LinkedList<>();
        for (List<Atom> l : dups) flat.addAll(l);

        try {
            addView(params.getQueries().customView(flat, params.getFilter()), params);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }

    private List<List<Atom>> getDuplicates(final AtomGraph graph,
                                       final Filter filter) throws RequestProcessingException {
        Map<String, List<Atom>> m = new HashMap<>();
        List<List<Atom>> dups = new LinkedList<>();
        int total = 0;

        for (Atom a : graph.getAllAtoms()) {
            if (filter.isVisible(a)) {
                String value = a.getValue();
                if (null != value && 0 < value.length()) {
                    String hash;
                    try {
                        hash = md5SumOf(value);
                    } catch (UnsupportedEncodingException e) {
                        throw new RequestProcessingException(e);
                    }
                    List<Atom> atoms = m.get(hash);
                    if (null == atoms) {
                        atoms = new LinkedList<>();
                        m.put(hash, atoms);
                    } else {
                        if (1 == atoms.size()) {
                            total++;
                            dups.add(atoms);
                        }

                        total++;
                        if (total > MAX_DUPLICATES) {
                            SemanticSynchrony.logInfo("showing only the first " + MAX_DUPLICATES + " duplicates");
                            break;
                        }
                    }

                    atoms.add(a);
                }
            }
        }

        return dups;
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
