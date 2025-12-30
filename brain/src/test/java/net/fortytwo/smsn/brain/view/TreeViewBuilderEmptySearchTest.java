package net.fortytwo.smsn.brain.view;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Tests for TreeViewBuilder handling of empty search results.
 */
public class TreeViewBuilderEmptySearchTest extends BrainTestBase {
    private AtomRepository repository;
    private TreeViewBuilder builder;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
        builder = new TreeViewBuilder(repository);
    }

    @Test
    public void testEmptySearchResultsDoesNotThrow() throws Exception {
        // Build search results view with empty list
        TreeNode result = builder.buildSearchResultsView(new ArrayList<>(), 1, Filter.noFilter());

        assertNotNull("Result should not be null", result);
        assertEquals("Result should have zero children", 0, result.children.size());
        assertEquals("Title should indicate no matches", "Search Results (no matches)", result.title);
    }

    @Test
    public void testEmptySearchResultsWithHeight() throws Exception {
        // Build search results view with empty list and various heights
        for (int height = 0; height <= 3; height++) {
            TreeNode result = builder.buildSearchResultsView(new ArrayList<>(), height, Filter.noFilter());

            assertNotNull("Result should not be null at height " + height, result);
            assertEquals("Result should have zero children at height " + height, 0, result.children.size());
        }
    }

    @Test
    public void testEmptySearchResultsWithDirection() throws Exception {
        // Build search results view with empty list and different directions
        TreeNode forwardResult = builder.buildSearchResultsView(
            new ArrayList<>(), 1, Filter.noFilter(), ViewDirection.FORWARD);
        TreeNode backwardResult = builder.buildSearchResultsView(
            new ArrayList<>(), 1, Filter.noFilter(), ViewDirection.BACKWARD);

        assertNotNull("Forward result should not be null", forwardResult);
        assertNotNull("Backward result should not be null", backwardResult);
        assertEquals("Forward result should have zero children", 0, forwardResult.children.size());
        assertEquals("Backward result should have zero children", 0, backwardResult.children.size());
    }
}
