package net.fortytwo.smsn.brain.query;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;

import java.util.LinkedList;
import java.util.List;

public interface ViewStyle {

    enum Direction {Forward, Backward}

    enum Basic {
        Forward("forward", createForwardStyle()),
        ForwardReadOnly("forward-add-only", createForwardAddOnlyStyle()),
        Backward("backward", createBackwardStyle());

        private final String name;
        private final ViewStyle style;

        Basic(final String name, final ViewStyle style) {
            this.name = name;
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

            throw new IllegalArgumentException("unknown view style: " + name);
        }

        private static ViewStyle createForwardStyle() {
            return new ViewStyle() {
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
                    return Basic.Backward.getStyle();
                }
            };
        }

        private static ViewStyle createForwardAddOnlyStyle() {
            return new ViewStyle() {
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
                    return Basic.Backward.getStyle();
                }
            };
        }

        private static ViewStyle createBackwardStyle() {
            return new ViewStyle() {
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
                    return Basic.Forward.getStyle();
                }
            };
        }
    }

    String getName();

    Iterable<Atom> getLinked(Atom root, Filter filter);

    boolean addOnUpdate();

    boolean deleteOnUpdate();

    Direction getDirection();

    ViewStyle getInverse();
}
