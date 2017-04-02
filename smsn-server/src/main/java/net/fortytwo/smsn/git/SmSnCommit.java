package net.fortytwo.smsn.git;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.AtomBase;
import net.fortytwo.smsn.brain.model.dto.ListDTO;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.diff.DiffEntry;
import org.eclipse.jgit.lib.ObjectReader;
import org.eclipse.jgit.lib.PersonIdent;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.treewalk.CanonicalTreeParser;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

public class SmSnCommit extends AtomBase {
    private final SmSnGitRepository repository;
    private final RevCommit commit;

    public SmSnCommit(SmSnGitRepository repository, final RevCommit commit) {
        this.repository = repository;
        this.commit = commit;
    }

    @Override
    public Long getCreated() {
        return commit.getCommitTime() * 1000L;
    }

    @Override
    public String getTitle() {
        String message = commit.getFullMessage().trim();

        String dateLabel = SmSnGitRepository.formatDate(getCreated());
        String authorLabel = getPersonLabel(getAuthorOrCommitter(commit));

        return dateLabel + " " + authorLabel + ": " + message;
    }

    @Override
    public Float getSharability() {
        return repository.getSharability();
    }

    @Override
    public EntityList<Atom> getChildren() {
        Optional<RevCommit> parent = getParent(commit);
        if (!parent.isPresent()) return null;

        try {
            return getDiffs(parent.get(), commit);
        } catch (IOException | GitAPIException e) {
            throw new RuntimeException(e);
        }
    }

    private String getPersonLabel(final PersonIdent person) {
        Preconditions.checkNotNull(person);
        String name = person.getName();
        String email = person.getEmailAddress();
        Preconditions.checkNotNull(name);
        return null == email ? name : name + " (" + email + ")";
    }

    private PersonIdent getAuthorOrCommitter(final RevCommit revCommit) {
        PersonIdent author = revCommit.getAuthorIdent();
        return null == author ? revCommit.getCommitterIdent() : author;
    }

    private Optional<RevCommit> getParent(final RevCommit commit) {
        RevCommit[] parents = commit.getParents();
        return 0 == parents.length ? Optional.empty() : Optional.of(parents[0]);
    }

    private EntityList<Atom> getDiffs(final RevCommit oldCommit, final RevCommit newCommit)
            throws IOException, GitAPIException {
        Git git = repository.getGit();
        EntityList<Atom> list = null, cur = null;

        ObjectReader reader = git.getRepository().newObjectReader();
        CanonicalTreeParser oldTreeIter = new CanonicalTreeParser();
        oldTreeIter.reset(reader, oldCommit.getTree().getId());
        CanonicalTreeParser newTreeIter = new CanonicalTreeParser();
        newTreeIter.reset(reader, newCommit.getTree().getId());
        List<DiffEntry> diffs = git.diff()
                .setNewTree(newTreeIter)
                .setOldTree(oldTreeIter)
                .call();

        for (DiffEntry diffEntry : diffs) {
            // note: ignored
            DiffEntry.ChangeType changeType = diffEntry.getChangeType();
            String oldPath = diffEntry.getOldPath();
            int score = diffEntry.getScore();

            //System.out.println("diff: " + diffEntry.toString());

            String newPath = diffEntry.getNewPath();

            String id = SmSnGitRepository.toId(changeType == DiffEntry.ChangeType.DELETE ? oldPath : newPath);
            Atom changedAtom = changedAtom(id, SmSnGitRepository.getTimeStamp(newCommit), changeType);

            list = creatList(changedAtom, list);
        }

        return list;
    }

    private EntityList<Atom> creatList(final Atom first, final EntityList<Atom> rest) {
        return new ListDTO<>(first, rest);
    }

    private Atom changedAtom(final String id, final long timestamp, final DiffEntry.ChangeType changeType) {
        Optional<Atom> opt = repository.getBrain().getTopicGraph().getAtomById(id);
        Atom atom;

        if (opt.isPresent()) {
            atom = opt.get();
        } else {
            atom = new AtomBase();
            atom.setId(id);
            atom.setCreated(timestamp);
            atom.setTitle(SmSnGitRepository.titleForMissingAtom(changeType));
            atom.setWeight(SemanticSynchrony.Weight.DEFAULT);
            atom.setSharability(this.getSharability());
        }

        return atom;
    }
}
