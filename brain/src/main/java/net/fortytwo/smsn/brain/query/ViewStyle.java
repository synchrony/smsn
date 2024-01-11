package net.fortytwo.smsn.brain.query;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.ListNode;

import java.util.LinkedList;
import java.util.List;

public interface ViewStyle {

    enum Direction {Forward, Backward}

    enum Basic {
        Forward(createForwardStyle()),
        ForwardAddOnly(createForwardAddOnlyStyle()),
        Backward(createBackwardStyle());

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

            throw new IllegalArgumentException("unknown view style: " + name);
        }

        private static ViewStyle createForwardStyle() {
            return new ViewStyle() {
                @Override
                public String getName() {
                    return "forward";
                }

                @Override
                public Iterable<Note> getLinked(final Note root,
                                                final Filter filter) {
                    return TreeViews.toFilteredIterable(root.getChildren(), filter);
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
                public Iterable<Note> getLinked(final Note root,
                                                final Filter filter) {
                    return TreeViews.toFilteredIterable(root.getChildren(), filter);
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
                public Iterable<Note> getLinked(final Note root,
                                                final Filter filter) {
                    List<Note> results = new LinkedList<>();
                    root.forFirstOf(list -> {
                        ListNode<Note> cur = list;
                        ListNode<Note> prev = null;
                        while (null != cur) {
                            prev = cur;
                            cur = cur.getRestOf();
                        }

                        Note a = root.getSubject(prev);
                        if (filter.test(a)) {
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

    Iterable<Note> getLinked(Note root, Filter filter);

    boolean addOnUpdate();

    boolean deleteOnUpdate();

    Direction getDirection();

    ViewStyle getInverse();
}
