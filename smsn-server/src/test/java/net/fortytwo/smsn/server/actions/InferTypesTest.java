package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.ActionContext;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Tests for InferTypes action - runs type inference on the knowledge base.
 */
public class InferTypesTest extends ActionTestBase {

    @Before
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test
    public void inferTypesCompletesSuccessfully() throws Exception {
        InferTypes action = new InferTypes();

        // Should complete without error
        ActionContext context = perform(action);

        // The action doesn't return any specific data
        assertNotNull(context);
        assertNotNull(context.getMap());
    }

    @Test
    public void inferTypesCanBeCalledMultipleTimes() throws Exception {
        // Per the comment in InferTypes, multiple invocations may be required
        InferTypes action = new InferTypes();

        for (int i = 0; i < 4; i++) {
            ActionContext context = perform(action);
            assertNotNull(context);
        }
    }

    @Test
    public void inferTypesWithEmptyGraph() throws Exception {
        // Inference should work even on empty graph
        InferTypes action = new InferTypes();
        ActionContext context = perform(action);
        assertNotNull(context);
    }

    @Test
    public void inferTypesWithDataInGraph() throws Exception {
        // Create some atoms first
        brain.getAtomRepository().createAtom(
            new net.fortytwo.smsn.brain.AtomId("test1"),
            new net.fortytwo.smsn.brain.SourceName("private"),
            "Test Atom 1"
        );
        brain.getAtomRepository().createAtom(
            new net.fortytwo.smsn.brain.AtomId("test2"),
            new net.fortytwo.smsn.brain.SourceName("private"),
            "Test Atom 2"
        );

        InferTypes action = new InferTypes();
        ActionContext context = perform(action);
        assertNotNull(context);
    }
}
