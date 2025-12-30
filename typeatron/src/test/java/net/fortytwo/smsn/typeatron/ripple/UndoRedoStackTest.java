package net.fortytwo.smsn.typeatron.ripple;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class UndoRedoStackTest {

    @Test
    public void emptyStackCannotUndo() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        assertFalse(stack.canUndo());
        assertNull(stack.currentState());
    }

    @Test
    public void emptyStackCannotRedo() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        assertFalse(stack.canRedo());
    }

    @Test
    public void doneAddsToStack() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        stack.done("first");
        assertTrue(stack.canUndo());
        assertEquals("first", stack.currentState());
    }

    @Test
    public void undoMovesToRedoStack() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        stack.done("first");
        stack.done("second");

        assertEquals("second", stack.currentState());
        String undone = stack.undo();
        assertEquals("second", undone);
        assertEquals("first", stack.currentState());
        assertTrue(stack.canRedo());
    }

    @Test
    public void redoRestoresState() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        stack.done("first");
        stack.done("second");

        stack.undo();  // undoes "second"
        assertEquals("first", stack.currentState());

        String redone = stack.redo();
        assertEquals("second", redone);
        assertEquals("second", stack.currentState());
    }

    @Test
    public void doneClearsRedoStack() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        stack.done("first");
        stack.done("second");

        stack.undo();
        assertTrue(stack.canRedo());

        stack.done("third");  // Should clear redo stack
        assertFalse(stack.canRedo());
    }

    @Test
    public void capacityLimitsUndoHistory() {
        UndoRedoStack<Integer> stack = new UndoRedoStack<>(3);
        stack.done(1);
        stack.done(2);
        stack.done(3);
        stack.done(4);  // Should push out 1

        // Can only undo 3 times (4, 3, 2)
        assertEquals(Integer.valueOf(4), stack.currentState());
        stack.undo();
        assertEquals(Integer.valueOf(3), stack.currentState());
        stack.undo();
        assertEquals(Integer.valueOf(2), stack.currentState());
        stack.undo();
        assertNull(stack.currentState());  // Can't undo further
        assertFalse(stack.canUndo());
    }

    @Test(expected = IllegalStateException.class)
    public void undoThrowsWhenEmpty() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        stack.undo();
    }

    @Test(expected = IllegalStateException.class)
    public void redoThrowsWhenEmpty() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        stack.done("first");
        stack.redo();  // Nothing to redo
    }

    @Test
    public void multipleUndoRedo() {
        UndoRedoStack<String> stack = new UndoRedoStack<>(10);
        stack.done("a");
        stack.done("b");
        stack.done("c");

        // Undo twice
        stack.undo();  // c
        stack.undo();  // b
        assertEquals("a", stack.currentState());

        // Redo once
        stack.redo();  // brings back b
        assertEquals("b", stack.currentState());

        // New action clears redo
        stack.done("d");
        assertEquals("d", stack.currentState());
        assertFalse(stack.canRedo());
    }
}
