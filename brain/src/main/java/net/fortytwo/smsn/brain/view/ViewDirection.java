package net.fortytwo.smsn.brain.view;

/**
 * Direction for tree view traversal.
 * Replaces the legacy ViewStyle concept with a simpler enum.
 */
public enum ViewDirection {
    /**
     * Forward view: expand children of the root atom.
     */
    FORWARD,

    /**
     * Backward view: expand parents of the root atom.
     */
    BACKWARD;

    /**
     * Get the inverse direction.
     */
    public ViewDirection getInverse() {
        return this == FORWARD ? BACKWARD : FORWARD;
    }
}
