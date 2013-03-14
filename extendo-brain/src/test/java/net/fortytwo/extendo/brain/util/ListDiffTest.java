package net.fortytwo.extendo.brain.util;

import net.fortytwo.extendo.brain.NoteQueries;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ListDiffTest {
    private Comparator<String> cmp = new ListDiff.StringComparator();

    private String findDiff(final String before,
                            final String after) {
        List<String> beforeList = ListDiff.toList(before);
        List<String> afterList = ListDiff.toList(after);

        List<String> lcs = ListDiff.leastCommonSubsequence(beforeList, afterList, cmp);
        return ListDiff.toString(lcs);
    }

    private int additions;
    private int removals;

    private String applyDiff(final String before,
                             final String after,
                             final String diff) throws NoteQueries.InvalidUpdateException {
        additions = 0;
        removals = 0;

        List<String> beforeList = ListDiff.toList(before);
        List<String> afterList = ListDiff.toList(after);
        List<String> diffList = ListDiff.toList(diff);

        final List<String> work = new LinkedList<String>();
        work.addAll(beforeList);

        ListDiff.DiffEditor<String> ed = new ListDiff.DiffEditor<String>() {
            public void add(int position, String letter) {
                //System.out.println("" + position + ": +" + letter);
                work.add(position, letter);
                additions++;
            }

            public void delete(int position, String letter) {
                //System.out.println("" + position + ": -" + letter);
                work.remove(position);
                removals++;
            }
        };

        Comparator<String> cmp = new ListDiff.StringComparator();
        ListDiff.applyDiff(beforeList, afterList, diffList, cmp, ed);

        return ListDiff.toString(work);
    }

    @Before
    public void setUp() {

    }

    @After
    public void tearDown() {

    }

    @Test
    public void testSubstitutionAtEndOfString() throws Exception {
        String before = "atg";
        String after = "aty";

        String diff = findDiff(before, after);
        assertEquals("at", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(1, additions);
        assertEquals(1, removals);
    }

    @Test
    public void testAddAll() throws Exception {
        String before = "";
        String after = "abcde";

        String diff = findDiff(before, after);
        assertEquals("", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(5, additions);
        assertEquals(0, removals);
    }

    @Test
    public void testRemoveAll() throws Exception {
        String before = "abcde";
        String after = "";

        String diff = findDiff(before, after);
        assertEquals("", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(0, additions);
        assertEquals(5, removals);
    }

    @Test
    public void testTrivial() throws Exception {
        String before = "";
        String after = "";

        String diff = findDiff(before, after);
        assertEquals("", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(0, additions);
        assertEquals(0, removals);
    }

    @Test
    public void testNoDifference() throws Exception {
        String before = "abcde";
        String after = "abcde";

        String diff = findDiff(before, after);
        assertEquals("abcde", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(0, additions);
        assertEquals(0, removals);
    }

    @Test
    public void testSimpleRemoval() throws Exception {
        String before = "abcde";
        String after = "acde";

        String diff = findDiff(before, after);
        assertEquals("acde", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(0, additions);
        assertEquals(1, removals);
    }

    @Test
    public void testConsecutiveRemoval() throws Exception {
        String before = "abcde";
        String after = "abc";

        String diff = findDiff(before, after);
        assertEquals("abc", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(0, additions);
        assertEquals(2, removals);
    }

    @Test
    public void testNonconsecutiveRemoval() throws Exception {
        String before = "abcde";
        String after = "ace";

        String diff = findDiff(before, after);
        assertEquals("ace", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(0, additions);
        assertEquals(2, removals);
    }

    @Test
    public void testSimpleSubstitution() throws Exception {
        String before = "...X..";
        String after = "...Y..";

        String diff = findDiff(before, after);
        assertEquals(".....", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(1, additions);
        assertEquals(1, removals);
    }

    @Test
    public void testSwapAdjacent() throws Exception {
        String before = "...XY..";
        String after = "...YX..";

        String diff = findDiff(before, after);
        assertEquals("...X..", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(1, additions);
        assertEquals(1, removals);
    }

    @Test
    public void testReverse() throws Exception {
        String before = "...ABC..";
        String after = "...CBA..";

        String diff = findDiff(before, after);
        assertEquals("...A..", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(2, additions);
        assertEquals(2, removals);
    }

    @Test
    public void testNothingInCommonSameLength() throws Exception {
        String before = "1";
        String after = "2";

        String diff = findDiff(before, after);
        assertEquals("", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(1, additions);
        assertEquals(1, removals);
    }

    @Test
    public void testNothingInCommonFirstIsShorter() throws Exception {
        String before = "abc";
        String after = "df;lkfgjh";

        String diff = findDiff(before, after);
        assertEquals("", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(9, additions);
        assertEquals(3, removals);
    }

    @Test
    public void testNothingInCommonFirstIsLonger() throws Exception {
        String before = "yeurytuwii";
        String after =  "26738";

        String diff = findDiff(before, after);
        assertEquals("", diff);

        assertEquals(after, applyDiff(before, after, diff));
        assertEquals(5, additions);
        assertEquals(10, removals);
    }
}
