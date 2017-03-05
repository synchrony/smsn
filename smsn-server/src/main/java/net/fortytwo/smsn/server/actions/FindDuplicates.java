package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.server.ActionContext;
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
public class FindDuplicates extends FilteredAction {

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
    protected void performTransaction(final ActionContext context) throws RequestProcessingException {
        List<List<Atom>> dups = getDuplicates(context.getBrain().getTopicGraph(), filter);
        List<Atom> flat = new LinkedList<>();
        for (List<Atom> l : dups) flat.addAll(l);

        try {
            addView(context.getQueries().customView(flat, filter), context);
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

    private List<List<Atom>> getDuplicates(final TopicGraph graph,
                                           final Filter filter) throws RequestProcessingException {
        Map<String, List<Atom>> m = new HashMap<>();
        List<List<Atom>> dups = new LinkedList<>();
        int total = 0;

        for (Atom a : graph.getAllAtoms()) {
            if (filter.isVisible(a)) {
                String title = a.getTitle();
                if (null != title && 0 < title.length()) {
                    String hash;
                    try {
                        hash = md5SumOf(title);
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
