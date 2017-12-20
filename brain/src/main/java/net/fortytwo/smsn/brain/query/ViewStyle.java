package net.fortytwo.smsn.brain.query;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.styles.TopicParents;
import net.fortytwo.smsn.brain.query.styles.TopicChildrenAddOnly;
import net.fortytwo.smsn.brain.query.styles.TopicChildren;
import net.fortytwo.smsn.brain.query.styles.NoteChildren;

import java.util.function.Consumer;

public interface ViewStyle {

    enum Direction {Forward, Backward}

    enum Basic {
        Simple(new NoteChildren()),
        Forward(new TopicChildren()),
        ForwardAddOnly(new TopicChildrenAddOnly()),
        Backward(new TopicParents());

        private final ViewStyle style;

        Basic(final ViewStyle style) {
            this.style = style;
        }

        public ViewStyle getStyle() {
            return style;
        }

        public static ViewStyle lookup(final String name) {
            for (Basic style : values()) {
                if (name.equals(style.getStyle().getName())) {
                    return style.getStyle();
                }
            }

            throw new IllegalArgumentException("unknown " + ViewStyle.class.getName() + ": " + name);
        }
    }

    String getName();

    void visitLinked(Note root, Filter filter, Consumer<Note> visitor);

    boolean addOnUpdate();

    boolean deleteOnUpdate();

    boolean deleteRecursively();

    Direction getDirection();

    ViewStyle getInverse();
}
