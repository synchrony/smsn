package net.fortytwo.extendo.brain.rdf;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface BottomUpType {
    Field[] getFields();

    /**
     * @return a built-in constraint on the value of instances of this type.
     * It may be overridden in certain contexts.
     * If null, there is no context-independent constraint.
     */
    Pattern getValueRegex();

    boolean aliasRequired();

    boolean childrenRequired();
}
