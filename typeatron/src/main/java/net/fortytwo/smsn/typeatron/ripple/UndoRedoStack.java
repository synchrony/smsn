package net.fortytwo.smsn.typeatron.ripple;

import java.util.Stack;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class UndoRedoStack<T> {
    private final Stack<T> undoStack;
    private final Stack<T> redoStack;

    private final int capacity;

    public UndoRedoStack(final int capacity) {
        this.capacity = capacity;
        undoStack = new Stack<T>();
        redoStack = new Stack<T>();
    }

    public void done(T t) {
        redoStack.clear();
        if (undoStack.size() >= capacity) {
            undoStack.remove(0);
        }
        undoStack.push(t);
    }

    public T currentState() {
        return canUndo() ? undoStack.peek() : null;
    }

    public boolean canUndo() {
        return undoStack.size() > 0;
    }

    public boolean canRedo() {
        return redoStack.size() > 0;
    }

    public T undo() {
        if (!canUndo()) {
            throw new IllegalStateException();
        }

        T t = undoStack.pop();
        redoStack.push(t);
        return t;
    }

    public T redo() {
        if (!canRedo()) {
            throw new IllegalStateException();
        }

        T t = undoStack.pop();
        undoStack.push(t);
        return t;
    }
}
