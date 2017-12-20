package net.fortytwo.smsn.git;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
import net.fortytwo.smsn.config.DataSource;
import org.eclipse.jgit.api.errors.GitAPIException;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class RepositoryCollection implements AbstractRepository {

    private final File directory;

    private final Brain brain;
    private final Map<String, SmSnGitRepository> repositoriesBySource;

    public RepositoryCollection(final Brain brain, final File directory) throws IOException {
        this.brain = brain;

        Preconditions.checkNotNull(directory);
        Preconditions.checkArgument(directory.exists());
        Preconditions.checkArgument(directory.isDirectory());
        this.directory = directory;

        repositoriesBySource = new HashMap<>();
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            repositoriesBySource.put(source.getName(), createRepository(source));
        }
    }

    @Override
    public void addAll() throws RepositoryException {
        forEach(AbstractRepository::addAll);
    }

    @Override
    public void commitAll(String message) throws RepositoryException {
        forEach(repo -> repo.commitAll(message));
    }

    @Override
    public void pull() throws RepositoryException {
        forEach(AbstractRepository::pull);
    }

    @Override
    public void push() throws RepositoryException {
        forEach(AbstractRepository::push);
    }

    @Override
    public void cycle(final String message) throws RepositoryException {
        forEach(repo -> repo.cycle(message));
    }

    @Override
    public Set<String> getAdded() throws RepositoryException {
        return unionOf(AbstractRepository::getAdded);
    }

    @Override
    public Set<String> getRemoved() throws RepositoryException {
        return unionOf(AbstractRepository::getRemoved);
    }

    @Override
    public Set<String> getUntracked() throws RepositoryException {
        return unionOf(AbstractRepository::getUntracked);
    }

    @Override
    public Set<String> getChanged() throws RepositoryException {
        return unionOf(AbstractRepository::getChanged);
    }

    @Override
    public Set<String> getModified() throws RepositoryException {
        return unionOf(AbstractRepository::getModified);
    }

    @Override
    public Set<String> getConflicting() throws RepositoryException {
        return unionOf(AbstractRepository::getConflicting);
    }

    @Override
    public Set<String> getMissing() throws RepositoryException {
        return unionOf(AbstractRepository::getMissing);
    }

    public Note getHistory(final SmSnGitRepository.Limits limits) throws IOException, GitAPIException {
        long now = System.currentTimeMillis();

        NoteDTO parent = new NoteDTO();
        Model.setTopicId(parent, SemanticSynchrony.createRandomId());
        // TODO: don't hard-code a source
        parent.setSource("public");
        parent.setWeight(SemanticSynchrony.DEFAULT_WEIGHT);
        parent.setCreated(now);

        parent.setLabel("Git history for " + directory.getName()
                + " at " + SmSnGitRepository.formatDate(now));

        int index = 0;
        for (SmSnGitRepository repo : repositoriesBySource.values()) {
            Note repoHistory = repo.getHistory(limits);
            parent.addChild(index++, repoHistory);
        }

        parent.setNumberOfChildren(repositoriesBySource.size());

        return parent;
    }

    private <R> Set<R> unionOf(FunctionWithException<AbstractRepository, Set<R>, RepositoryException> function)
            throws RepositoryException {
        Set<R> result = new HashSet<>();
        for (SmSnGitRepository repo : repositoriesBySource.values()) {
            result.addAll(function.apply(repo));
        }
        return result;
    }

    private void forEach(final ConsumerWithException<AbstractRepository, RepositoryException> consumer)
            throws RepositoryException {
        for (SmSnGitRepository repo : repositoriesBySource.values()) {
            consumer.accept(repo);
        }
    }

    private SmSnGitRepository createRepository(final DataSource dataSource) throws IOException {
        return new SmSnGitRepository(brain, dataSource);
    }

    private interface ConsumerWithException<T, E extends Exception> {
        void accept(T t) throws E;
    }

    private interface FunctionWithException<D, R, E extends Exception> {
        R apply(D d) throws E;
    }
}
