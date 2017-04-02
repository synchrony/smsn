package net.fortytwo.smsn.git;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

public class SmSnGitRepositoryIT extends BrainTestBase  {

    @Override
    protected TopicGraph createAtomGraph() throws IOException {
        return createTinkerAtomGraph();
    }

    @Test
    public void thingsDontBreak() throws IOException, GitAPIException {
        SmSnGitRepository repo = new SmSnGitRepository(
                brain, new File("/tmp/universal"), SemanticSynchrony.Sharability.UNIVERSAL);
        Note history = repo.getHistory(SmSnGitRepository.Limits.noLimits());

        System.out.println("history: " + history);
    }
}
