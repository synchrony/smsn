package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.view.TreeUpdate;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.Assert.*;

/**
 * Regression tests for the entity-bullet data-loss bug (2026-09-04).
 *
 * smsn-mode's choose-bullet emits U+25C6 (a diamond) for atoms that have a
 * Hydra entity record, REPLACING the + or the middot the atom would
 * otherwise carry. BULLET_PATTERN did not list the diamond, so every such
 * line failed to match, parseLine returned quietly, and the atom never
 * entered the parsed tree. The updater then wrote that tree back --
 * deleting every entity atom from the view, with no error at any point.
 *
 * Reported symptom: a "log of movies" view came back holding only its
 * three non-diamond children.
 */
public class TreeNodeWikiParserBulletTest {

    private TreeNodeWikiParser parser;

    @Before
    public void setUp() {
        parser = new TreeNodeWikiParser();
    }

    private TreeUpdate parse(String wiki) throws Exception {
        try (InputStream in = new ByteArrayInputStream(
                wiki.getBytes(StandardCharsets.UTF_8))) {
            return parser.parseUpdate(in);
        }
    }

    @Test
    public void diamondBulletedEntityAtomsSurvive() throws Exception {
        // The reported view, reduced: two entity atoms and one plain child.
        TreeUpdate u = parse(
                "◆ :Inx39F2Hy9gBJgEF: Joker: Folie a deux\n"
              + "◆ :mX6FPtDEScnlSdnn: The Godfather\n"
              + "+ :sRBpyk3EfzwhwEBX: The Phantom Tollbooth (film)\n");

        assertEquals("every bulleted child must survive the round trip",
                3, u.getTree().children.size());
        assertEquals("Joker: Folie a deux",
                u.getTree().children.get(0).title);
    }

    @Test
    public void allBulletTypesParse() throws Exception {
        TreeUpdate u = parse(
                "* a brand-new atom\n"
              + "+ :aaaaaaaaaaaaaaaa: an expanded existing atom\n"
              + "· :bbbbbbbbbbbbbbbb: a collapsed existing atom\n"
              + "◆ :cccccccccccccccc: an entity atom\n");

        assertEquals(4, u.getTree().children.size());
    }

    /**
     * The deeper defect: an unrecognized line was silently dropped. Any
     * future bullet added to the view would delete data the same way, so
     * the parser must refuse rather than guess.
     */
    @Test
    public void unknownBulletIsRejectedLoudly() throws Exception {
        try {
            parse("★ :dddddddddddddddd: a star-bulleted atom\n");
            fail("an unrecognized bullet must not be silently ignored");
        } catch (IllegalArgumentException expected) {
            assertTrue(expected.getMessage(),
                    expected.getMessage().contains("Unparseable line"));
        }
    }

    /** Strict rejection must not break multi-line titles, which the view
     *  wraps in {@code {{{ }}}} (13 such atoms in the current store). */
    @Test
    public void multiLineTitlesStillParse() throws Exception {
        TreeUpdate u = parse(
                "◆ :eeeeeeeeeeeeeeee: {{{\n"
              + "first line\n"
              + "second line\n"
              + "}}}\n");

        assertEquals(1, u.getTree().children.size());
    }
}
