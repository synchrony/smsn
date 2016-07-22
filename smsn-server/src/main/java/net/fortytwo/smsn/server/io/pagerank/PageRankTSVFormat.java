package net.fortytwo.smsn.server.io.pagerank;

import net.fortytwo.smsn.server.io.Format;

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
