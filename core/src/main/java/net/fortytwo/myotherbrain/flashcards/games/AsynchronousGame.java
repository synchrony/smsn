package net.fortytwo.myotherbrain.flashcards.games;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Game;
import net.fortytwo.myotherbrain.flashcards.GameplayException;
import net.fortytwo.myotherbrain.flashcards.Pile;
import net.fortytwo.myotherbrain.flashcards.Trial;
import net.fortytwo.myotherbrain.flashcards.db.GameHistory;

/**
 * User: josh
 * Date: 3/29/11
 * Time: 2:31 PM
 */
public abstract class AsynchronousGame<Q, A> extends Game<Q, A> {
    private Card<Q, A> current;

    public AsynchronousGame(final Pile<Q, A> pile,
                            final GameHistory history) {
        super(pile, history);
    }

    public abstract void nextQuestion(Card<Q, A> current);

    @Override
    public void play() throws GameplayException {
        nextCard();
    }

    private void nextCard() {
        // Only if no cards have yet been drawn, or if the previous card has been replaced, do we draw a new card.
        // This is not the case at the beginning of the game, or when the user has left and subsequently restarted the game.
        if (null == current) {
            current = drawCard();
        }

        nextQuestion(current);
    }

    public void correct() throws GameplayException {
        logAndReplace(current, Trial.Result.Correct);
        current = null;
        nextCard();
    }

    public void incorrect() throws GameplayException {
        logAndReplace(current, Trial.Result.Correct);
        current = null;
        nextCard();
    }
}
