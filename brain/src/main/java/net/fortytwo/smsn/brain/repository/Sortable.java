package net.fortytwo.smsn.brain.repository;

/**
 * A wrapper for entities that have a score for sorting.
 * Used for search results where entities need to be ranked by relevance.
 */
public class Sortable<E, S extends Comparable<S>> implements Comparable<Sortable<E, S>> {

    private final E entity;
    private final S score;

    public Sortable(E entity, S score) {
        this.entity = entity;
        this.score = score;
    }

    public E getEntity() {
        return entity;
    }

    public S getScore() {
        return score;
    }

    @Override
    public int compareTo(Sortable<E, S> other) {
        // Descending order - higher scores first
        return other.score.compareTo(this.score);
    }
}
