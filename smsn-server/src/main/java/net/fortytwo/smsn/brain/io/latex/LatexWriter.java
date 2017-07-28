package net.fortytwo.smsn.brain.io.latex;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.query.ViewStyle;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class LatexWriter extends NoteWriter {

    private static final int MAX_LATEX_RECURSE_LEVELS = 16;

    private final Serializer[] serializers = new Serializer[]{
            new QuotedValueSerializer(),
            new VerbatimBlockSerializer(),
            new CommentSerializer(),
            new SectionSerializer(),
    };

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(LatexFormat.getInstance());
    }

    @Override
    public void doWrite(Context context) throws IOException {
        String rootId = context.getRootId();
        Preconditions.checkNotNull(rootId, "root id is required");
        Filter filter = context.getFilter();

        Optional<Note> opt = context.getTopicGraph().getNotesById(rootId);
        if (!opt.isPresent()) {
            throw new IllegalStateException("no such atom: " + rootId);
        }

        writeLatex(opt.get(), filter, 0, 0, context.getDestStream());
    }

    private void writeLatex(final Note root,
                            final Filter filter,
                            final int level,
                            final int sectionLevel,
                            final OutputStream out) throws IOException {

        if (!filter.test(root)) {
            return;
        }

        if (level >= MAX_LATEX_RECURSE_LEVELS) {
            logger.warning("LaTeX tree exceeds maximum depth of " + MAX_LATEX_RECURSE_LEVELS);
            return;
        }

        // trim immediately; don't try to preserve indentation or trailing whitespace
        String value = root.getTitle().trim();

        for (Serializer serializer : serializers) {
            if (serializer.matches(value)) {
                SerializerIn input = new SerializerIn(value, sectionLevel);
                SerializerOut output = serializer.serialize(input);

                out.write(output.getText().getBytes());
                out.write('\n');

                if (output.isRecursive()) {
                    for (Note child : ViewStyle.Basic.Forward.getStyle().getLinked(root, filter)) {
                        writeLatex(child, filter, level + 1, output.isSection() ? sectionLevel + 1 : sectionLevel, out);
                    }
                }

                break;
            }
        }
    }

    private static class SerializerIn {
        private final String currentValue;
        private final int sectionLevel;

        private SerializerIn(String currentValue, int sectionLevel) {
            this.currentValue = currentValue;
            this.sectionLevel = sectionLevel;
        }

        public String getCurrentValue() {
            return currentValue;
        }

        public int getSectionLevel() {
            return sectionLevel;
        }
    }

    private static class SerializerOut {
        private String text;
        private boolean isRecursive;
        private boolean isSection;

        public String getText() {
            return text;
        }

        public boolean isRecursive() {
            return isRecursive;
        }

        public boolean isSection() {
            return isSection;
        }

        public void setText(String text) {
            this.text = text;
        }

        public void setRecursive(boolean isRecursive) {
            this.isRecursive = isRecursive;
        }

        public void setIsSection(boolean isSection) {
            this.isSection = isSection;
        }
    }

    private interface Serializer {
        boolean matches(String value);

        SerializerOut serialize(SerializerIn input);
    }

    private static class QuotedValueSerializer implements Serializer {

        @Override
        public boolean matches(String value) {
            return value.startsWith("\"");
        }

        @Override
        public SerializerOut serialize(SerializerIn input) {
            String value = input.getCurrentValue();
            SerializerOut out = new SerializerOut();
            out.setText(value.substring(1, value.endsWith("\"") ? value.length() - 1 : value.length()));
            return out;
        }
    }

    private static class VerbatimBlockSerializer implements Serializer {

        @Override
        public boolean matches(String value) {
            return value.contains("\\n");
        }

        @Override
        public SerializerOut serialize(SerializerIn input) {
            // write verbatim blocks out verbatim
            // note: we don't expect any children of verbatim blocks
            SerializerOut out = new SerializerOut();
            out.setText(input.getCurrentValue());
            return out;
        }
    }

    private static class CommentSerializer implements Serializer {

        @Override
        public boolean matches(String value) {
            return value.startsWith("%");
        }

        @Override
        public SerializerOut serialize(SerializerIn input) {
            // Add an extra newline before demarcated paragraphs.
            // This saves on explicit line breaks in the source notes.
            SerializerOut out = new SerializerOut();
            out.setRecursive(true);
            out.setText("\n" + input.getCurrentValue());
            return out;
        }
    }

    private class SectionSerializer implements Serializer {

        private final Set<String>
                sectionKeywords,
                nonbreakingKeywords;

        protected SectionSerializer() {
            sectionKeywords = keywords("section", "subsection", "subsubsection");
            nonbreakingKeywords = keywords("begin", "caption", "end", "item", "label", "textbf");
        }

        @Override
        public boolean matches(String value) {
            return value.startsWith("\\");
        }

        @Override
        public SerializerOut serialize(SerializerIn input) {
            String value = input.getCurrentValue();

            SerializerOut out = new SerializerOut();
            out.setRecursive(true);

            // Automatically correct section/subsection/subsubsection keywords according to the hierarchy;
            // this allows more flexibility w.r.t. including content trees in multiple documents.
            for (String keyword : sectionKeywords) {
                if (value.startsWith(keyword)) {
                    String replacement = input.getSectionLevel() > 1
                            ? "\\subsubsection" : input.getSectionLevel() > 0
                            ? "\\subsection" : "\\section";
                    value = replacement + value.substring(keyword.length());
                    out.setIsSection(true);
                    break;
                }
            }

            // for a few keywords, like /item, we don't need the extra space
            boolean doBreak = true;
            if (!out.isSection()) {
                for (String keyword : nonbreakingKeywords) {
                    if (value.startsWith(keyword)) {
                        doBreak = false;
                        break;
                    }
                }
            }

            // also add an extra newline before Exobrain items which are
            // specifically LaTeX, e.g. chapters, sections, subsections, begin blocks.
            out.setText(doBreak ? "\n" + value : value);
            return out;
        }

        private Set<String> keywords(final String... labels) {
            Set<String> set = new HashSet<>();
            for (String l : labels) {
                set.add("\\" + l);
            }

            return set;
        }
    }
}
