package net.fortytwo.smsn.brain.io.vertices;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.brain.util.OptHelper;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

public class VertexWriter extends NoteWriter {
    private static final Logger logger = Logger.getLogger(VertexWriter.class.getName());

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(VertexTSVFormat.getInstance());
    }

    @Override
    public void doWrite(Context context) throws IOException {

        AtomRepository repository = context.getAtomRepository();
        Filter filter = context.getFilter();
        Preconditions.checkNotNull(filter);
        KnowledgeBase sourceKb = context.getKnowledgeBase();
        PrintStream p = new PrintStream(context.getDestStream());

        p.println("created\tid\tweight\tpriority\tsource\tclass\tout\tin\ttitle\talias\tshortcut\ttext");

        for (AtomId atomId : repository.getAllAtomIds()) {
            Atom atom = repository.load(atomId);
            if (isTrueAtom(atom) && repository.testFilter(atom, filter)) {
                p.print(atom.created.value);
                p.print('\t');
                p.print(atom.id.value);
                p.print('\t');
                p.print(atom.weight.value);
                p.print('\t');
                // Priority: Opt<Normed> - use helper to extract
                p.print(extractPriority(atom));
                p.print('\t');
                p.print(atom.source.value);
                p.print('\t');

                // TODO: Update KnowledgeBase to work with Atom instead of Note
                // For now, skip class info
                p.print("\t0\t0\t");

                if (null == atom.title) {
                    logger.warning("atom has null title: " + atom.id.value);
                } else {
                    p.print(escapeValue(atom.title));
                }
                p.print('\t');

                p.print(extractAlias(atom));
                p.print('\t');

                p.print(extractShortcut(atom));
                p.print('\t');

                p.print(extractText(atom));

                p.print('\n');
            }
        }
    }

    private boolean isTrueAtom(final Atom atom) {
        return atom.created != null;
    }

    private String extractPriority(Atom atom) {
        try {
            // Use reflection to avoid importing Opt
            Object priorityObj = atom.getClass().getField("priority").get(atom);
            Boolean isPresent = (Boolean) priorityObj.getClass().getMethod("isPresent").invoke(priorityObj);
            if (isPresent) {
                Object normed = priorityObj.getClass().getMethod("get").invoke(priorityObj);
                Object value = normed.getClass().getField("value").get(normed);
                return escapeValue(String.valueOf(value));
            }
        } catch (Exception e) {
            // Ignore
        }
        return "";
    }

    private String extractAlias(Atom atom) {
        return extractOptString(atom, "alias");
    }

    private String extractShortcut(Atom atom) {
        return extractOptString(atom, "shortcut");
    }

    private String extractText(Atom atom) {
        return extractOptString(atom, "text");
    }

    private String extractOptString(Atom atom, String fieldName) {
        try {
            // Use reflection to avoid importing Opt
            Object optObj = atom.getClass().getField(fieldName).get(atom);
            Boolean isPresent = (Boolean) optObj.getClass().getMethod("isPresent").invoke(optObj);
            if (isPresent) {
                Object value = optObj.getClass().getMethod("get").invoke(optObj);
                return escapeValue(String.valueOf(value));
            }
        } catch (Exception e) {
            // Ignore
        }
        return "";
    }

    // Note: quote characters (") need to be replaced, e.g. with underscores (_), if this data is imported into R.
    // Otherwise, R becomes confused and skips rows.
    private String escapeValue(final String value) {
        return SemanticSynchrony.unicodeEscape(value);
    }
}
