package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.dto.ListNodeDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import org.junit.Test;

import java.util.function.Predicate;

import static org.junit.Assert.assertEquals;

public class FilterIndexTest {
    private final Predicate<Integer> filter = i -> i != 0;

    @Test
    public void nilListGetsZero() {
        assertEquals(0, indexFor(0, new Integer[]{}));
        assertEquals(0, indexFor(1, new Integer[]{}));
    }

    @Test
    public void emptyListGetsUpperLimit() {
        assertEquals(1, indexFor(0, new Integer[]{0}));
        assertEquals(1, indexFor(1, new Integer[]{0}));
        assertEquals(2, indexFor(0, new Integer[]{0, 0}));
        assertEquals(2, indexFor(1, new Integer[]{0, 0}));
        assertEquals(2, indexFor(2, new Integer[]{0, 0}));
    }

    @Test
    public void zeroMatchesFirst() {
        assertEquals(0, indexFor(0, new Integer[]{1}));
        assertEquals(1, indexFor(0, new Integer[]{0, 1}));
    }

    @Test
    public void oneGoesBeyondFirst() {
        assertEquals(1, indexFor(1, new Integer[]{1}));
        assertEquals(2, indexFor(1, new Integer[]{0, 1}));
        assertEquals(3, indexFor(1, new Integer[]{0, 1, 0}));
    }

    @Test
    public void tailIsIgnored() {
        assertEquals(0, indexFor(0, new Integer[]{1, 0}));
        assertEquals(1, indexFor(0, new Integer[]{0, 1, 0}));
    }

    @Test
    public void onesIncrementCount() {
        assertEquals(0, indexFor(0, new Integer[]{1, 1}));
        assertEquals(1, indexFor(1, new Integer[]{1, 1}));
        assertEquals(2, indexFor(2, new Integer[]{1, 1}));
        assertEquals(1, indexFor(0, new Integer[]{0, 1, 1, 0}));
        assertEquals(2, indexFor(1, new Integer[]{0, 1, 1, 0}));
        assertEquals(4, indexFor(2, new Integer[]{0, 1, 1, 0}));
    }

    private int indexFor(final int position, final Integer[] array) {
        ListNode<Integer> list = toEntityList(array);
        return TreeViews.indexOfNthVisible(list, position, filter);
    }

    private ListNode<Integer> toEntityList(final Integer[] array) {
        return ListNodeDTO.fromArray(array);
    }
}
