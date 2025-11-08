package net.fortytwo.smsn.git;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.Timestamp;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.config.DataSource;
import org.eclipse.jgit.api.errors.GitAPIException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
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

    public TreeNode getHistory(final SmSnGitRepository.Limits limits) throws IOException, GitAPIException {
        long now = System.currentTimeMillis();

        List<TreeNode> children = new ArrayList<>();
        for (SmSnGitRepository repo : repositoriesBySource.values()) {
            TreeNode repoHistory = repo.getHistory(limits);
            children.add(repoHistory);
        }

        return TreeViewBuilder.createSimpleTreeNode(
                SemanticSynchrony.createRandomId(),
                new Timestamp((int) (now / 1000)),
                new Normed(SemanticSynchrony.DEFAULT_WEIGHT),
                new SourceName("public"),  // TODO: don't hard-code a source
                "Git history for " + directory.getName() + " at " + SmSnGitRepository.formatDate(now),
                children,
                children.size(),
                0
        );
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
        return SmSnGitRepository.createRepository(brain, dataSource);
    }

    private interface ConsumerWithException<T, E extends Exception> {
        void accept(T t) throws E;
    }

    private interface FunctionWithException<D, R, E extends Exception> {
        R apply(D d) throws E;
    }
}
