package net.fortytwo.smsn.git;

import org.eclipse.jgit.api.errors.GitAPIException;

import java.util.Set;

public interface AbstractRepository {
    void addAll() throws RepositoryException;
    void commitAll(String message) throws RepositoryException;
    void pull() throws RepositoryException;
    void push() throws RepositoryException;
    void cycle(String message) throws RepositoryException;
    Set<String> getAdded() throws RepositoryException;
    Set<String> getRemoved() throws RepositoryException;
    Set<String> getUntracked() throws RepositoryException;
    Set<String> getChanged() throws RepositoryException;
    Set<String> getModified() throws RepositoryException;
    Set<String> getConflicting() throws RepositoryException;
    Set<String> getMissing() throws RepositoryException;

    class RepositoryException extends Exception {
        public RepositoryException(final Throwable cause) {
            super(cause);
        }

        public RepositoryException(final String message) {
            super(message);
        }
    }
}
