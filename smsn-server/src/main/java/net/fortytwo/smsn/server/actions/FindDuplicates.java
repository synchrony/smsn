package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
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
import java.util.logging.Level;

/**
 * A service for identifying notes with duplicate values.
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
        List<List<Note>> dups = getDuplicates(context.getBrain().getTopicGraph(), getFilter());
        List<Note> flat = new LinkedList<>();
        dups.forEach(flat::addAll);

        try {
            addView(context.getQueries().customView(flat, getFilter()), context);
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

    private List<List<Note>> getDuplicates(final TopicGraph graph,
                                           final Filter filter) throws RequestProcessingException {
        Map<String, List<Note>> m = new HashMap<>();
        List<List<Note>> dups = new LinkedList<>();
        int total = 0;

        for (Note a : graph.getAllNotes()) {
            if (filter.test(a)) {
                String title = Note.getTitle(a);
                if (null != title && 0 < title.length()) {
                    String hash;
                    try {
                        hash = md5SumOf(title);
                    } catch (UnsupportedEncodingException e) {
                        throw new RequestProcessingException(e);
                    }
                    List<Note> notes = m.get(hash);
                    if (null == notes) {
                        notes = new LinkedList<>();
                        m.put(hash, notes);
                    } else {
                        if (1 == notes.size()) {
                            total++;
                            dups.add(notes);
                        }

                        total++;
                        if (total > MAX_DUPLICATES) {
                            SemanticSynchrony.getLogger().log(Level.INFO, "showing only the first "
                                    + MAX_DUPLICATES + " duplicates");
                            break;
                        }
                    }

                    notes.add(a);
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
