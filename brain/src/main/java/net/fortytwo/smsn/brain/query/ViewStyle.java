package net.fortytwo.smsn.brain.query;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;

import java.util.LinkedList;
import java.util.List;

public interface ViewStyle {

    enum Direction {Forward, Backward}

    ViewStyle[] ALL_STYLES = {ViewStyle.FORWARD, ViewStyle.FORWARD_ADD_ONLY, ViewStyle.BACKWARD};

    static ViewStyle lookupStyle(final String name) {
        for (ViewStyle style : ALL_STYLES) {
            if (name.equals(style.getName())) {
                return style;
            }
        }

        throw new IllegalArgumentException("unknown view style: " + name);
    }

    String getName();

    Iterable<Atom> getLinked(Atom root, Filter filter);

    boolean addOnUpdate();

    boolean deleteOnUpdate();

    Direction getDirection();

    ViewStyle getInverse();

    ViewStyle FORWARD = new ViewStyle() {
        @Override
        public String getName() {
            return "forward";
        }

        @Override
        public Iterable<Atom> getLinked(final Atom root,
                                        final Filter filter) {
            return TreeViews.toIterable(root.getNotes());
        }

        @Override
        public boolean addOnUpdate() {
            return true;
        }

        @Override
        public boolean deleteOnUpdate() {
            return true;
        }

        @Override
        public Direction getDirection() {
            return Direction.Forward;
        }

        @Override
        public ViewStyle getInverse() {
            return BACKWARD;
        }
    };

    ViewStyle FORWARD_ADD_ONLY = new ViewStyle() {
        @Override
        public String getName() {
            return "forward-add-only";
        }

        @Override
        public Iterable<Atom> getLinked(final Atom root,
                                        final Filter filter) {
            return TreeViews.toIterable(root.getNotes());
        }

        @Override
        public boolean addOnUpdate() {
            return true;
        }

        @Override
        public boolean deleteOnUpdate() {
            return false;
        }

        @Override
        public Direction getDirection() {
            return Direction.Forward;
        }

        @Override
        public ViewStyle getInverse() {
            return BACKWARD;
        }
    };

    ViewStyle BACKWARD = new ViewStyle() {
        @Override
        public String getName() {
            return "backward";
        }

        @Override
        public Iterable<Atom> getLinked(final Atom root,
                                        final Filter filter) {
            List<Atom> results = new LinkedList<>();
            root.forFirstOf(list -> {
                EntityList<Atom> cur = list;
                EntityList<Atom> prev = null;
                while (null != cur) {
                    prev = cur;
                    cur = cur.getRestOf();
                }

                Atom a = prev.getFirst().getSubject(prev);
                if (filter.isVisible(a)) {
                    results.add(a);
                }
            });

            return results;
        }

        @Override
        public boolean addOnUpdate() {
            return false;
        }

        @Override
        public boolean deleteOnUpdate() {
            return false;
        }

        @Override
        public Direction getDirection() {
            return Direction.Backward;
        }

        @Override
        public ViewStyle getInverse() {
            return FORWARD;
        }
    };
}
