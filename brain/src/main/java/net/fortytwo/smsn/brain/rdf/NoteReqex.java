package net.fortytwo.smsn.brain.rdf;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class NoteReqex {

    public enum Modifier {
        ZeroOrOne, ZeroOrMore, One, OneOrMore
    }

    private final List<El> elements;

    public NoteReqex(List<El> elements) {
        this.elements = elements;
    }

    public List<El> getElements() {
        return elements;
    }

    public static class El {
        private final NoteClass.FieldHandler fieldHandler;
        private final Modifier modifier;
        private final Set<Class<? extends NoteClass>> alternatives;
        private final int weight;

        public El(final int weight,
                  final NoteClass.FieldHandler fieldHandler,
                  final Modifier modifier,
                  final Class<? extends NoteClass>... alternatives) {
            this.weight = weight;
            this.fieldHandler = fieldHandler;
            this.modifier = modifier;
            this.alternatives = new HashSet<>();
            Collections.addAll(this.alternatives, alternatives);
        }

        public El(final NoteClass.FieldHandler fieldHandler,
                  final Modifier modifier,
                  final Class<? extends NoteClass>... alternatives) {
            this(0 == alternatives.length ? 0 : 1, fieldHandler, modifier, alternatives);
        }

        public NoteClass.FieldHandler getFieldHandler() {
            return fieldHandler;
        }

        public Modifier getModifier() {
            return modifier;
        }

        public Set<Class<? extends NoteClass>> getAlternatives() {
            return alternatives;
        }

        public int getWeight() {
            return weight;
        }
    }
}
