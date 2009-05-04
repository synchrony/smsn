package net.fortytwo.myotherbrain.undo;

import junit.framework.TestCase;

/**
 * Author: josh
 * Date: May 4, 2009
 * Time: 3:23:08 AM
 */
public class UndoStackTest extends TestCase {
    private String currentValue = "undefined";

    public void testNormalExecution() {
        UndoStack stack = new UndoStack(3);

        assertEquals("undefined", currentValue);
        assertFalse(stack.canUndo());
        assertFalse(stack.canRedo());

        // do
        stack.addAndDo(new SimpleAction("one"));
        assertEquals("one", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // do
        stack.addAndDo(new SimpleAction("two"));
        assertEquals("two", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // undo
        stack.undo();
        assertEquals("one", currentValue);
        assertTrue(stack.canUndo());
        assertTrue(stack.canRedo());

        // redo
        stack.redo();
        assertEquals("two", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // undo both
        stack.undo();
        stack.undo();
        assertEquals("undefined", currentValue);
        assertFalse(stack.canUndo());
        assertTrue(stack.canRedo());

        // do
        stack.addAndDo(new SimpleAction("three"));
        assertEquals("three", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // do until the buffer is full
        stack.addAndDo(new SimpleAction("four"));
        stack.addAndDo(new SimpleAction("five"));
        assertEquals("five", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // do once more
        stack.addAndDo(new SimpleAction("six"));
        assertEquals("six", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // undo until at the beginning of the buffer
        stack.undo();
        stack.undo();
        stack.undo();
        assertEquals("three", currentValue);
        assertFalse(stack.canUndo());
        assertTrue(stack.canRedo());

        // redo until at the end of the buffer
        stack.redo();
        stack.redo();
        stack.redo();
        assertEquals("six", currentValue);
        assertTrue(stack.canUndo());
        assertFalse(stack.canRedo());

        // clear
        stack.clear();
        assertEquals("six", currentValue);
        assertFalse(stack.canUndo());
        assertFalse(stack.canRedo());
    }

    public void testExceptions() {
        UndoStack stack;

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
            stack.undo();
            assertTrue(false);
        } catch (IllegalStateException e) {
        }
        stack.addAndDo(new SimpleAction("A"));
        try {
            stack.redo();
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

        protected void executeUndo() {
            currentValue = memento;
        }

        protected void executeRedo() {
            memento = currentValue;

            currentValue = newValue;
        }

        public String toString() {
            return "(\"" + newValue + "\")";
        }
    }
}
