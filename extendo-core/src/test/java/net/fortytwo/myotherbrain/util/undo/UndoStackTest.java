package net.fortytwo.myotherbrain.util.undo;

import junit.framework.TestCase;

public class UndoStackTest extends TestCase {
    private String currentValue = "undefined";

    public void testNormalExecution() throws Exception {
        UndoStack stack = new UndoStack(3);
        Object c = "";

        assertEquals("undefined", currentValue);
        assertFalse(stack.canUndo());
        assertFalse(stack.canRedo());

        // do
        stack.addAndDo(new SimpleAction("one"), c);
        assertEquals("one", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // do
        stack.addAndDo(new SimpleAction("two"), c);
        assertEquals("two", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // undo
        stack.undo(c);
        assertEquals("one", currentValue);
        assertTrue(stack.canUndo());
        assertTrue(stack.canRedo());

        // redo
        stack.redo(c);
        assertEquals("two", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // undo both
        stack.undo(c);
        stack.undo(c);
        assertEquals("undefined", currentValue);
        assertFalse(stack.canUndo());
        assertTrue(stack.canRedo());

        // do
        stack.addAndDo(new SimpleAction("three"), c);
        assertEquals("three", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // do until the buffer is full
        stack.addAndDo(new SimpleAction("four"), c);
        stack.addAndDo(new SimpleAction("five"), c);
        assertEquals("five", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // do once more
        stack.addAndDo(new SimpleAction("six"), c);
        assertEquals("six", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // undo until at the beginning of the buffer
        stack.undo(c);
        stack.undo(c);
        stack.undo(c);
        assertEquals("three", currentValue);
        assertFalse(stack.canUndo());
        assertTrue(stack.canRedo());

        // redo until at the end of the buffer
        stack.redo(c);
        stack.redo(c);
        stack.redo(c);
        assertEquals("six", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // clear
        stack.clear();
        assertEquals("six", currentValue);
        assertFalse(stack.canUndo());
        assertFalse(stack.canRedo());
    }

    public void testExceptions() throws Exception {
        UndoStack stack;
        Object c = "";

        try {
            stack = new UndoStack(-42);
            assertTrue(false);
        } catch (IllegalArgumentException e) {
        }

        try {
            stack = new UndoStack(0);
            assertTrue(false);
        } catch (IllegalArgumentException e) {
        }

        stack = new UndoStack(1);
        try {
            stack.undo(c);
            assertTrue(false);
        } catch (IllegalStateException e) {
        }
        stack.addAndDo(new SimpleAction("A"), c);
        try {
            stack.redo(c);
            assertTrue(false);
        } catch (IllegalStateException e) {
        }
    }

    private class SimpleAction extends UndoableAction {
        private final String newValue;
        private String memento;

        public SimpleAction(final String newValue) {
            this.newValue = newValue;
        }

        protected void executeUndo(Object t) {
            currentValue = memento;
        }

        protected void executeRedo(Object t) {
            memento = currentValue;

            currentValue = newValue;
        }

        public String toString() {
            return "(\"" + newValue + "\")";
        }
    }
}
