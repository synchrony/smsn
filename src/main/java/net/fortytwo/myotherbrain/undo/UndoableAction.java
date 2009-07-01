package net.fortytwo.myotherbrain.undo;

/**
 * Framework for undoable actions using the Command pattern.  Instances of this class should contain all information
 * necessary to carry out an action.  For now, any mementos necessary to undo an action shall be gathered at the time
 * the command object is constructed (the other options being: at the time redo() is first called, or prior to
 *  construction such that the memento is then passed to the constructor as an argument).
 *
 * Author: josh
 * Date: May 4, 2009
 * Time: 12:04:13 AM
 */
public abstract class UndoableAction<T> {
    private enum State { DONE, UNDONE }

    private State currentState = State.UNDONE;

    protected abstract void executeUndo(T t);

    protected abstract void executeRedo(T t);

    /**
     * @throws IllegalStateException if the call to this method does not follow a call to redo()
     * @param t
     */
    public void undo(T t) {
        if (State.DONE != currentState) {
            throw new IllegalStateException("action has not yet been done: " + this);
        }
        
        executeUndo(t);

        currentState = State.UNDONE;
    }

    /**
     * @throws IllegalStateException if the call to this method follows a previous call to redo(), without an intermediate undo()
     * @param t
     */
    public void redo(T t) {
        if (State.UNDONE != currentState) {
            throw new IllegalStateException("action has alredy been done: " + this);
        }

        executeRedo(t);

        currentState = State.DONE;
    }
}
