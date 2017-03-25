package net.fortytwo.smsn.git;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.io.vcs.VCSFormat;
import net.fortytwo.smsn.brain.model.Note;
import org.eclipse.jgit.api.errors.GitAPIException;

import java.io.File;
import java.io.IOException;

public class RepositoryCollection {

    private final File directory;

    private final Brain brain;
    private final SmSnGitRepository[] repositories;

    public RepositoryCollection(final Brain brain, final File directory) throws IOException {
        this.brain = brain;

        Preconditions.checkNotNull(directory);
        Preconditions.checkArgument(directory.exists());
        Preconditions.checkArgument(directory.isDirectory());
        this.directory = directory;

        SmSnGitRepository privateRepo = createRepository(VCSFormat.DirectoryNames.PRIVATE);
        SmSnGitRepository personalRepo = createRepository(VCSFormat.DirectoryNames.PERSONAL);
        SmSnGitRepository publicRepo = createRepository(VCSFormat.DirectoryNames.PUBLIC);
        SmSnGitRepository universalRepo = createRepository(VCSFormat.DirectoryNames.UNIVERSAL);

        repositories = new SmSnGitRepository[]{universalRepo, publicRepo, personalRepo, privateRepo};
    }

    public Note getHistory(final SmSnGitRepository.Limits limits) throws IOException, GitAPIException {
        long now = System.currentTimeMillis();

        Note parent = new Note();
        parent.setId(SemanticSynchrony.createRandomId());
        parent.setSharability(SemanticSynchrony.Sharability.PUBLIC);
        parent.setWeight(SemanticSynchrony.Weight.DEFAULT);
        parent.setCreated(now);

        parent.setTitle("Git history for " + directory.getName()
                + " at " + SmSnGitRepository.formatDate(now));

        for (SmSnGitRepository repo : repositories) {
            Note repoHistory = repo.getHistory(limits);
            parent.addChild(repoHistory);
        }

        parent.setNumberOfChildren(repositories.length);

        return parent;
    }

    private SmSnGitRepository createRepository(final String name) throws IOException {
        return new SmSnGitRepository(brain, new File(directory, name));
    }
}
