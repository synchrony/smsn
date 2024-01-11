package net.fortytwo.smsn.git;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

public class RepositoryCollectionIT extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void thingsDontBreak() throws IOException, GitAPIException {
        SmSnGitRepository.Limits limits = new SmSnGitRepository.Limits();
        limits.setMaxDiffsPerRepository(10);
        limits.setMaxFilesPerDiff(10);

        RepositoryCollection repo = new RepositoryCollection(brain, new File("/Users/josh/data/smsn/brains/git-smsn"));
        TreeNode<Link> history = repo.getHistory(limits);

        System.out.println("history: " + history);
    }
}
