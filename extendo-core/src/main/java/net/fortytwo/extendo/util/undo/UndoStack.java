package net.fortytwo.extendo.util.undo;

/**
 * A "stack" of undoable actions with a fixed capacity.  It is possible to execute new actions, to undo them, and to
 * redo them in a linear, single-user fashion.
 * Note: not synchronized for use in multiple threads.
 * <p/>
 * Author: josh
 * Date: May 4, 2009
 * Time: 1:59:17 AM
 */
public class UndoStack<T, E extends Exception> {
    private final UndoableAction<T, E>[] stack;
    private final int capacity;

    private int headIndex;

    private int totalActions;
    private int actionsDone;

    /**
     * Creates a new stack.
     * @param capacity the capacity of the stack.  After the stack contains this many actions, old actions will
     * necessarily be over-written with new ones, in FIFO order.
     */
    public UndoStack(final int capacity) {
        if (capacity < 1) {
            throw new IllegalArgumentException("capacity must be greater than 0");
        }

        this.capacity = capacity;
        stack = new UndoableAction[capacity];

        clear();
    }

    /**
     * Clears the stack of all actions.
     */
    public void clear() {
        headIndex = 0;
        totalActions = 0;
        actionsDone = 0;
    }

    /**
     * @return whether there are any previously done actions to undo
     */
    public boolean canUndo() {
        return actionsDone > 0;
    }

    /**
     * @return whether there are any previously undone actions to redo
     */
    public boolean canRedo() {
        return actionsDone < totalActions;
    }

    /**
     * Adds an action and executes (does) it.  It is then on the top of the stack and is the first action to be undone
     * with a call to undo().
     * @param action the action to execute
     */
    public void addAndDo(final UndoableAction<T, E> action,
                         final T t) throws E {
        stack[(headIndex + actionsDone) % capacity] = action;

        if (actionsDone < capacity) {
            actionsDone++;
        } else {
            headIndex = (headIndex + 1) % capacity;
        }

        totalActions = actionsDone;

        // Executing the action is the last thing we do, in case of
        // uncaught Exceptions.
        action.redo(t);
    }

    /**
     * Undoes the last action previously done.
     * @throws IllegalStateException if there are no actions to undo
     */
    public void undo(final T t) throws E {
        if (!canUndo()) {
            throw new IllegalStateException("no actions to undo");
        } else {
            actionsDone--;
            UndoableAction<T, E> action = stack[(headIndex + actionsDone) % capacity];

            // Executing the action is the last thing we do, in case of
            // uncaught Exceptions.
            action.undo(t);
        }
    }

    /**
     * Redoes the last action previously undone.
     * @throws IllegalStateException if there are no actions to redo
     */
    public void redo(final T t) throws E {
        if (!canRedo()) {
            throw new IllegalStateException("no actions to redo");
        } else {
            UndoableAction<T, E> action = stack[(headIndex + actionsDone) % capacity];
            actionsDone++;

            // Executing the action is the last thing we do, in case of
            // uncaught Exceptions.
            action.redo(t);
        }
    }
}
