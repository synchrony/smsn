package net.fortytwo.smsn.brain.model.pg;

class Sortable<T, C extends Comparable<C>> implements Comparable<Sortable<T, C>> {
    private final T entity;
    private C score;

    public Sortable(T entity, C score) {
        this.entity = entity;
        this.score = score;
    }

    @Override
    public int compareTo(Sortable<T, C> other) {
        // order by decreasing score
        return other.getScore().compareTo(score);
    }

    public T getEntity() {
        return entity;
    }

    public C getScore() {
        return score;
    }

    public void setScore(C score) {
        this.score = score;
    }
}
