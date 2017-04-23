package net.fortytwo.smsn.git;

public interface AbstractRepository {
    void addAll() throws RepositoryException;
    void commitAll(String message) throws RepositoryException;
    void pull() throws RepositoryException;
    void push() throws RepositoryException;
    void cycle(String message) throws RepositoryException;

    class RepositoryException extends Exception {
        public RepositoryException(final Throwable cause) {
            super(cause);
        }

        public RepositoryException(final String message) {
            super(message);
        }
    }
}
