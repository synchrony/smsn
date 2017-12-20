package net.fortytwo.smsn.git;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.io.wiki.WikiPrinter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
import net.fortytwo.smsn.config.DataSource;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.RemoteAddCommand;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.transport.URIish;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.util.Iterator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class SmSnGitRepositoryTest extends BrainTestBase {
    private File repoDir;
    private SmSnGitRepository repo;
    private Git git;

    @Before
    public void setUp() throws Exception {
        super.setUp();

        repoDir = createTempDirectory();
        init();

        DataSource dataSource = new DataSource();
        dataSource.setName(DefaultSources.PUBLIC);
        dataSource.setLocation(repoDir.getAbsolutePath());

        repo = new SmSnGitRepository(brain, dataSource);
        git = repo.getGit();
        assertNotNull(git.getRepository().getRef(Constants.HEAD));
        assertTrue(git.status().call().isClean());

        addOrigin();
    }

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void simpleAddAllIsCorrect() throws Exception {
        assertEquals(0, countUntracked());
        assertEquals(0, countAdded());

        addFile(testNote(ARTHUR_ID, "Arthur Dent"));
        assertEquals(1, countUntracked());
        assertEquals(0, countAdded());

        repo.addAll();
        assertEquals(0, countUntracked());
        assertEquals(1, countAdded());

        addFile(testNote(FORD_ID, "Ford Prefect"));
        assertEquals(1, countUntracked());
        addFile(testNote(ZAPHOD_ID, "Zaphod Beeblebrox"));
        assertEquals(2, countUntracked());
        assertEquals(1, countAdded());

        repo.addAll();
        assertEquals(0, countUntracked());
        assertEquals(3, countAdded());
    }

    @Test
    public void commitIsCorrect() throws Exception {
        addFile(testNote(ARTHUR_ID, "Arthur Dent"));
        repo.addAll();
        assertEquals(0, countCommits());
        assertEquals(1, countAdded());

        repo.commitAll("First commit");
        assertEquals(1, countCommits());
        assertEquals(0, countAdded());
        assertEquals("First commit", git.log().call().iterator().next().getFullMessage());
    }

    @Test
    public void historyDoesntBreak() throws Exception {
        String arthurId = SemanticSynchrony.createRandomId();
        String fordId = SemanticSynchrony.createRandomId();
        String zaphodId = SemanticSynchrony.createRandomId();

        addFile(testNote(arthurId, "Arthur Dent"));
        repo.addAll();
        repo.commitAll("first commit");
        addFile(testNote(fordId, "Ford Prefect"));
        addFile(testNote(zaphodId, "Zaphod Beeblebrox"));
        repo.addAll();
        repo.commitAll("second commit");

        Note history = repo.getHistory(SmSnGitRepository.Limits.noLimits());
        assertEquals(2, ListNode.lengthOf(history.getFirst()));
        Note secondCommit = history.getFirst().get(0);
        Note firstCommit = history.getFirst().get(1);
        assertTrue(firstCommit.getLabel().endsWith("first commit"));
        assertTrue(secondCommit.getLabel().endsWith("second commit"));
        /* TODO: test interned notes
        assertEquals(1, firstCommit.getChildren().size());
        assertEquals(2, secondCommit.getChildren().size());
        assertEquals("Arthur Dent", firstCommit.getChildren().get(0).getTitle());
        */
    }

    private int countUntracked() throws GitAPIException {
        return git.status().call().getUntracked().size();
    }

    private int countAdded() throws AbstractRepository.RepositoryException {
        return repo.getAdded().size();
    }

    private boolean hasCommits() throws IOException {
        Ref headRef = git.getRepository().getRef( Constants.HEAD );
        return (headRef != null) && (headRef.getObjectId() != null);
    }

    private int countCommits() throws GitAPIException, IOException {
        if (!hasCommits()) return 0;

        Iterator<RevCommit> commits = git.log().call().iterator();
        int count = 0;
        while (commits.hasNext()) {
            count++;
            commits.next();
        }

        return count;
    }

    private Note testNote(final String id, final String title) {
        Note note = new NoteDTO();

        Model.setTopicId(note, id);
        note.setLabel(title);
        note.setSource(DefaultSources.PUBLIC);
        note.setWeight(SemanticSynchrony.DEFAULT_WEIGHT);
        note.setCreated(System.currentTimeMillis());

        return note;
    }

    private void addFile(final Note note) throws IOException {
        assertNotNull(Model.getTopicId(note));
        File file = new File(repoDir, Model.getTopicId(note));
        try (OutputStream out = new FileOutputStream(file)) {
            new WikiPrinter(out).print(note);
        }
    }

    private void init() throws GitAPIException {
        Git.init().setDirectory(repoDir).call();
        File gitDir = new File(repoDir, ".git");
        assertTrue(gitDir.exists());
        assertTrue(gitDir.isDirectory());
    }

    private void addOrigin() throws URISyntaxException, GitAPIException {
        RemoteAddCommand add = git.remoteAdd();
        add.setName("origin");
        String uri = "https://example.org/repo";
        add.setUri(new URIish(uri));
        add.call();
        assertEquals("origin", git.remoteList().call().iterator().next().getName());
        assertEquals(uri, git.remoteList().call().iterator().next().getURIs().iterator().next().toString());
    }
}
