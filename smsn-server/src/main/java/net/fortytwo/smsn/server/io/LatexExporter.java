package net.fortytwo.smsn.server.io;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.ExtendoBrain;
import net.fortytwo.smsn.brain.Filter;
import net.fortytwo.smsn.brain.NoteQueries;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class LatexExporter extends Exporter {

    private static final int MAX_LATEX_RECURSE_LEVELS = 16;

    private static final Set<String>
            sectionKeywords,
            nonbreakingKeywords;

    static {
        sectionKeywords = keywords("section", "subsection", "subsubsection");
        nonbreakingKeywords = keywords("begin", "caption", "end", "item", "label", "textbf");
    }

    public static final String FORMAT = "LaTeX";

    @Override
    public List<String> getFormats() {
        return Arrays.asList(FORMAT);
    }

    @Override
    protected void exportInternal(ExtendoBrain sourceBrain, OutputStream destStream)
            throws IOException {
        String rootId = requireRootId();
        Filter filter = requireFilter();

        Atom rootAtom = sourceBrain.getBrainGraph().getAtom(rootId);
        if (null == rootAtom) {
            throw new IllegalStateException("no such atom: " + rootId);
        }

        writeLatex(rootAtom, filter, 0, 0, destStream);
    }

    private static Set<String> keywords(final String... labels) {
        Set<String> set = new HashSet<>();
        for (String l : labels) {
            set.add("\\" + l);
        }

        return set;
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
}
