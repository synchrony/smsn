package net.fortytwo.myotherbrain.flashmob.query;

import net.fortytwo.myotherbrain.flashmob.model.FlashMOBFirstClassItem;

/**
 * Author: josh
 * Date: Jul 23, 2009
 * Time: 2:21:56 PM
 */
public class FlashMOBQueryResult implements Comparable<FlashMOBQueryResult> {
    private FlashMOBFirstClassItem subject;
    private float emphasis;

    // TODO: this should be supplemented with sourceURI
    private FlashMOBFirstClassItem source;

    // Orders results by decreasing emphasis
    public int compareTo(final FlashMOBQueryResult other) {
        return this.emphasis > other.emphasis
                ? -1
                : this.emphasis < other.emphasis
                ? 1
                : 0;
    }

    public FlashMOBFirstClassItem getSubject() {
        return subject;
    }

    public void setSubject(final FlashMOBFirstClassItem subject) {
        this.subject = subject;
    }

    public float getEmphasis() {
        return emphasis;
    }

    public void setEmphasis(final float emphasis) {
        this.emphasis = emphasis;
    }

    public FlashMOBFirstClassItem getSource() {
        return source;
    }

    public void setSource(final FlashMOBFirstClassItem source) {
        this.source = source;
    }
}
