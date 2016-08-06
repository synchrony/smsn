package net.fortytwo.smsn.brain.io.pagerank;

import net.fortytwo.smsn.brain.io.Format;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PageRankTSVFormat extends Format {
    private static final PageRankTSVFormat instance = new PageRankTSVFormat();

    private PageRankTSVFormat() {
        super("PageRank", new String[]{"tsv"});
    }

    public static PageRankTSVFormat getInstance() {
        return instance;
    }
}
