package net.fortytwo.smsn.brain.view;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Tests for TreeViewBuilder - the component that builds hierarchical views from atoms.
 */
public class TreeViewBuilderTest extends BrainTestBase {
    private AtomRepository repository;
    private TreeViewBuilder builder;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
        builder = new TreeViewBuilder(repository);
    }

    // ==================== buildView tests ====================

    @Test
    public void buildViewWithHeightZeroReturnsRootOnly() throws Exception {
        Atom root = createAtom("root");
        Atom child = createAtom("child");
        repository.addChildAt(root.id, child.id, 0);

        TreeNode view = builder.buildView(root.id, 0, Filter.noFilter());

        assertEquals(root.id, view.id);
        assertEquals("root", view.title);
        assertTrue(view.children.isEmpty());  // Height 0 = no children expanded
        assertEquals(1, (int) view.numberOfChildren);  // But reports 1 child exists
    }

    @Test
    public void buildViewWithHeightOneExpandsChildren() throws Exception {
        Atom root = createAtom("root");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");
        repository.addChildAt(root.id, child1.id, 0);
        repository.addChildAt(root.id, child2.id, 1);

        TreeNode view = builder.buildView(root.id, 1, Filter.noFilter());

        assertEquals(root.id, view.id);
        assertEquals(2, view.children.size());
        assertEquals("child1", view.children.get(0).title);
        assertEquals("child2", view.children.get(1).title);
    }

    @Test
    public void buildViewPreservesChildOrder() throws Exception {
        Atom root = createAtom("root");
        Atom a = createAtom("a");
        Atom b = createAtom("b");
        Atom c = createAtom("c");
        repository.addChildAt(root.id, a.id, 0);
        repository.addChildAt(root.id, b.id, 1);
        repository.addChildAt(root.id, c.id, 2);

        TreeNode view = builder.buildView(root.id, 1, Filter.noFilter());

        assertEquals(3, view.children.size());
        assertEquals("a", view.children.get(0).title);
        assertEquals("b", view.children.get(1).title);
        assertEquals("c", view.children.get(2).title);
    }

    @Test
    public void buildViewExpandsNestedChildren() throws Exception {
        Atom root = createAtom("root");
        Atom child = createAtom("child");
        Atom grandchild = createAtom("grandchild");
        repository.addChildAt(root.id, child.id, 0);
        repository.addChildAt(child.id, grandchild.id, 0);

        TreeNode view = builder.buildView(root.id, 2, Filter.noFilter());

        assertEquals(1, view.children.size());
        TreeNode childView = view.children.get(0);
        assertEquals("child", childView.title);
        assertEquals(1, childView.children.size());
        assertEquals("grandchild", childView.children.get(0).title);
    }

    @Test
    public void buildViewDetectsCycles() throws Exception {
        Atom a = createAtom("a");
        Atom b = createAtom("b");
        repository.addChildAt(a.id, b.id, 0);
        repository.addChildAt(b.id, a.id, 0);  // Creates cycle

        TreeNode view = builder.buildView(a.id, 5, Filter.noFilter());

        // Should not cause infinite loop
        assertEquals("a", view.title);
        assertEquals(1, view.children.size());
        assertEquals("b", view.children.get(0).title);
        // The cycle is detected per-branch, so b->a is expanded but a's children are not
        // (since a was visited in this branch)
        assertEquals(1, view.children.get(0).children.size());
        assertEquals("a", view.children.get(0).children.get(0).title);
        // The second "a" has no children because we've hit the cycle
        assertEquals(0, view.children.get(0).children.get(0).children.size());
    }

    @Test
    public void buildViewCountsParents() throws Exception {
        Atom parent1 = createAtom("parent1");
        Atom parent2 = createAtom("parent2");
        Atom child = createAtom("child");
        repository.addChildAt(parent1.id, child.id, 0);
        repository.addChildAt(parent2.id, child.id, 0);

        TreeNode view = builder.buildView(parent1.id, 1, Filter.noFilter());

        TreeNode childView = view.children.get(0);
        assertEquals(2, (int) childView.numberOfParents);  // child has 2 parents
    }

    // ==================== buildView BACKWARD direction tests ====================

    @Test
    public void buildViewBackwardExpandsParents() throws Exception {
        Atom grandparent = createAtom("grandparent");
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(grandparent.id, parent.id, 0);
        repository.addChildAt(parent.id, child.id, 0);

        TreeNode view = builder.buildView(child.id, 2, Filter.noFilter(), ViewDirection.BACKWARD);

        assertEquals("child", view.title);
        assertEquals(1, view.children.size());  // "children" in backward = parents
        assertEquals("parent", view.children.get(0).title);
        assertEquals(1, view.children.get(0).children.size());
        assertEquals("grandparent", view.children.get(0).children.get(0).title);
    }

    // ==================== buildListView tests ====================

    @Test
    public void buildListViewCreatesVirtualRoot() throws Exception {
        Atom a = createAtom("a");
        Atom b = createAtom("b");

        TreeNode view = builder.buildListView(Arrays.asList(a, b), Filter.noFilter());

        assertEquals("list-view", view.id.value);
        assertEquals("List View", view.title);
        assertEquals(2, view.children.size());
        assertEquals("a", view.children.get(0).title);
        assertEquals("b", view.children.get(1).title);
    }

    @Test
    public void buildListViewHandlesEmptyList() throws Exception {
        TreeNode view = builder.buildListView(new ArrayList<>(), Filter.noFilter());

        assertEquals("list-view", view.id.value);
        assertEquals("List View (empty)", view.title);
        assertTrue(view.children.isEmpty());
    }

    @Test
    public void buildListViewDoesNotExpandChildren() throws Exception {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        // Reload parent to get updated children list
        parent = repository.load(parent.id);

        TreeNode view = builder.buildListView(Arrays.asList(parent), Filter.noFilter());

        assertEquals(1, view.children.size());
        TreeNode parentView = view.children.get(0);
        assertEquals("parent", parentView.title);
        assertTrue(parentView.children.isEmpty());  // No children expanded in list view
        assertEquals(1, (int) parentView.numberOfChildren);  // But reports child count
    }

    // ==================== buildSearchResultsView tests ====================

    @Test
    public void buildSearchResultsViewCreatesVirtualRoot() throws Exception {
        Atom a = createAtom("result a");
        Atom b = createAtom("result b");

        TreeNode view = builder.buildSearchResultsView(Arrays.asList(a, b), 1, Filter.noFilter());

        assertEquals("search-results", view.id.value);
        assertEquals("Search Results", view.title);
        assertEquals(2, view.children.size());
    }

    @Test
    public void buildSearchResultsViewHandlesEmptyResults() throws Exception {
        TreeNode view = builder.buildSearchResultsView(new ArrayList<>(), 1, Filter.noFilter());

        assertEquals("search-results", view.id.value);
        assertEquals("Search Results (no matches)", view.title);
        assertTrue(view.children.isEmpty());
    }

    @Test
    public void buildSearchResultsViewExpandsChildren() throws Exception {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        // height 2 should expand one level of children (due to actualHeight = height - 1)
        TreeNode view = builder.buildSearchResultsView(Arrays.asList(parent), 2, Filter.noFilter());

        TreeNode parentView = view.children.get(0);
        assertEquals(1, parentView.children.size());
        assertEquals("child", parentView.children.get(0).title);
    }

    @Test
    public void buildSearchResultsViewDeduplicates() throws Exception {
        Atom a = createAtom("a");

        // Same atom twice in results
        TreeNode view = builder.buildSearchResultsView(Arrays.asList(a, a), 1, Filter.noFilter());

        // Should only appear once
        assertEquals(1, view.children.size());
    }

    // ==================== Filter tests ====================

    @Test
    public void buildViewFiltersLowWeightAtoms() throws Exception {
        Atom root = createAtom("root");
        Atom highWeight = createAtom("high weight");
        Atom lowWeight = createAtom("low weight");

        // Set different weights
        highWeight = highWeight.withWeight(new net.fortytwo.smsn.brain.Normed(0.8f));
        lowWeight = lowWeight.withWeight(new net.fortytwo.smsn.brain.Normed(0.2f));
        repository.save(highWeight);
        repository.save(lowWeight);

        repository.addChildAt(root.id, highWeight.id, 0);
        repository.addChildAt(root.id, lowWeight.id, 1);

        // Filter requires minimum weight of 0.5
        // Use "private" as source since that's what createAtom uses
        Filter weightFilter = new Filter(0.5f, 0.5f, "private", "private");
        TreeNode view = builder.buildView(root.id, 1, weightFilter);

        // Only high weight child should be visible
        assertEquals(1, view.children.size());
        assertEquals("high weight", view.children.get(0).title);
    }

    @Test
    public void buildViewWithNullFilterIncludesAll() throws Exception {
        Atom root = createAtom("root");
        Atom child = createAtom("child");
        repository.addChildAt(root.id, child.id, 0);

        TreeNode view = builder.buildView(root.id, 1, null);

        assertEquals(1, view.children.size());
    }

    // ==================== Helper method tests ====================

    @Test
    public void createSimpleTreeNodeWorks() throws Exception {
        AtomId id = new AtomId("test-id");
        net.fortytwo.smsn.brain.Timestamp created = new net.fortytwo.smsn.brain.Timestamp(12345L);
        net.fortytwo.smsn.brain.Normed weight = new net.fortytwo.smsn.brain.Normed(0.75f);
        net.fortytwo.smsn.brain.SourceName source = new net.fortytwo.smsn.brain.SourceName("private");

        TreeNode node = TreeViewBuilder.createSimpleTreeNode(
            id, created, weight, source, "Test Title",
            new ArrayList<>(), 5, 2
        );

        assertEquals("test-id", node.id.value);
        assertEquals(12345L, (long) node.created.value);
        assertEquals(0.75f, node.weight.value, 0.001f);
        assertEquals("private", node.source.value);
        assertEquals("Test Title", node.title);
        assertFalse(node.priority.isPresent());
        assertFalse(node.alias.isPresent());
        assertEquals(5, (int) node.numberOfChildren);
        assertEquals(2, (int) node.numberOfParents);
    }

    @Test
    public void createAtomHelperWorks() throws Exception {
        AtomId id = new AtomId("helper-test");
        List<AtomId> children = Arrays.asList(new AtomId("child1"), new AtomId("child2"));

        Atom atom = TreeViewBuilder.createAtom(
            id, 12345L, 0.75f, "private", "Test Atom", "Some text", children
        );

        assertEquals("helper-test", atom.id.value);
        assertEquals(12345L, (long) atom.created.value);
        assertEquals(0.75f, atom.weight.value, 0.001f);
        assertEquals("private", atom.source.value);
        assertEquals("Test Atom", atom.title);
        assertTrue(atom.text.isPresent());
        assertEquals("Some text", atom.text.get());
        assertEquals(2, atom.children.size());
    }

    @Test
    public void createAtomHelperHandlesNullText() throws Exception {
        Atom atom = TreeViewBuilder.createAtom(
            new AtomId("test"), 0L, 0.5f, "public", "Title", null, new ArrayList<>()
        );

        assertFalse(atom.text.isPresent());
    }

    // ==================== Helper methods ====================

    private Atom createAtom(String title) {
        AtomId id = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new net.fortytwo.smsn.brain.SourceName("private"), title);
    }
}
