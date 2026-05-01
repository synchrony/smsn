package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.view.TreeUpdate;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import static org.junit.Assert.*;

/**
 * Tests for the {@code parseUpdate} entry point, which preserves the
 * user-intent side-channel needed to avoid silently clobbering stored
 * properties when the user's submitted view doesn't address them.
 */
public class TreeNodeWikiParserUpdateTest {
    private TreeNodeWikiParser parser;

    @Before
    public void setUp() {
        parser = new TreeNodeWikiParser();
    }

    private TreeUpdate parse(String wiki) throws Exception {
        try (InputStream in = new ByteArrayInputStream(wiki.getBytes())) {
            return parser.parseUpdate(in);
        }
    }

    @Test
    public void rootWeightIsTouchedWhenWritten() throws Exception {
        TreeUpdate u = parse("@weight 0.75\n@title Root\n");
        assertTrue(u.isRootPropertyTouched(SemanticSynchrony.PropertyKeys.WEIGHT));
        assertTrue(u.isRootPropertyTouched(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals(0.75f, u.getTree().weight.value, 0.0001f);
    }

    @Test
    public void rootWeightIsNotTouchedWhenAbsent() throws Exception {
        TreeUpdate u = parse("@title Root\n* :childA: hello\n");
        assertFalse(u.isRootPropertyTouched(SemanticSynchrony.PropertyKeys.WEIGHT));
        assertTrue(u.isRootPropertyTouched(SemanticSynchrony.PropertyKeys.TITLE));
        // The tree's weight defaults to 0.5 (parser default), but the
        // side-channel correctly says the user did NOT address it.
        assertEquals(SemanticSynchrony.DEFAULT_WEIGHT, u.getTree().weight.value, 0.0001f);
    }

    @Test
    public void childPropertyTouchedSetIsCorrect() throws Exception {
        // The user wrote @weight on childA but not on childB.
        String wiki =
                "@title Root\n" +
                "* :childA: A title\n" +
                "    @weight 0.75\n" +
                "* :childB: B title\n";
        TreeUpdate u = parse(wiki);

        AtomId a = new AtomId("childA");
        AtomId b = new AtomId("childB");

        // childA: title and weight authored
        assertTrue(u.isPropertyTouched(a, SemanticSynchrony.PropertyKeys.TITLE));
        assertTrue(u.isPropertyTouched(a, SemanticSynchrony.PropertyKeys.WEIGHT));

        // childB: title authored, weight NOT authored
        assertTrue(u.isPropertyTouched(b, SemanticSynchrony.PropertyKeys.TITLE));
        assertFalse(u.isPropertyTouched(b, SemanticSynchrony.PropertyKeys.WEIGHT));
    }

    @Test
    public void bareIdReferenceMarksTitleAsNotTouched() throws Exception {
        // "* :childA: " with no title text after the id is the canonical
        // way to reference a child without restating its title. The
        // updater must NOT overwrite childA's stored title with empty.
        String wiki = "@title Root\n* :childA: \n";
        TreeUpdate u = parse(wiki);
        AtomId a = new AtomId("childA");
        assertFalse(u.isPropertyTouched(a, SemanticSynchrony.PropertyKeys.TITLE));
        assertFalse(u.isPropertyTouched(a, SemanticSynchrony.PropertyKeys.WEIGHT));
    }

    @Test
    public void collapsedBulletMarksAtomAsCollapsed() throws Exception {
        String wiki = "@title Root\n· :childA: A title\n* :childB: B title\n";
        TreeUpdate u = parse(wiki);

        AtomId a = new AtomId("childA");
        AtomId b = new AtomId("childB");

        // childA was rendered with the collapsed-bullet marker; updater
        // must not delete its stored children.
        assertTrue(u.isCollapsed(a));
        // childB was rendered with the open marker; its children list
        // (empty here) IS the user's assertion.
        assertFalse(u.isCollapsed(b));
    }

    @Test
    public void expandedBulletWithNoChildrenIsNotCollapsed() throws Exception {
        // The user wrote "+ :childA:" — the "expanded" marker with an
        // empty children block. This means "I am addressing childA's
        // children, and I want it to have none". The updater SHOULD
        // delete childA's stored children in this case.
        String wiki = "@title Root\n+ :childA: A title\n";
        TreeUpdate u = parse(wiki);
        AtomId a = new AtomId("childA");
        assertFalse(u.isCollapsed(a));
    }
}
