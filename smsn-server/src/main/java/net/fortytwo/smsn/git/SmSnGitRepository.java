package net.fortytwo.smsn.git;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.query.Model;
import net.fortytwo.smsn.brain.query.ViewStyle;
import net.fortytwo.smsn.config.DataSource;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.GitCommand;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.diff.DiffEntry;
import org.eclipse.jgit.lib.ObjectReader;
import org.eclipse.jgit.lib.PersonIdent;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.merge.MergeStrategy;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.storage.file.FileRepositoryBuilder;
import org.eclipse.jgit.treewalk.CanonicalTreeParser;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Logger;

public class SmSnGitRepository extends NoteDTO implements AbstractRepository {

    private static final Logger logger = Logger.getLogger(SmSnGitRepository.class.getName());

    private static final ThreadLocal<DateFormat> dateFormat = ThreadLocal.withInitial(
            () -> new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ"));

    private final DataSource dataSource;
    private final Repository repository;
    private final Git git;
    private final File directory;

    private final Brain brain;
    private final Model model;
    private final Filter filter;

    public static String formatDate(final long timeStamp) {
        return dateFormat.get().format(new Date(timeStamp));
    }

    public SmSnGitRepository(final Brain brain, final DataSource dataSource) throws IOException {
        super();

        this.brain = brain;
        this.dataSource = dataSource;
        model = new Model(brain);

        // TODO
        this.filter = Filter.noFilter();

        directory = new File(dataSource.getLocation());
        Preconditions.checkNotNull(directory);
        Preconditions.checkArgument(directory.exists());
        Preconditions.checkArgument(directory.isDirectory());

        File gitDirectory = new File(directory, ".git");
        Preconditions.checkArgument(gitDirectory.exists());
        Preconditions.checkArgument(gitDirectory.isDirectory());

        verifyCanRead();

        FileRepositoryBuilder builder = new FileRepositoryBuilder();
        repository = builder.setGitDir(gitDirectory)
                .readEnvironment() // scan environment GIT_* variables
                .findGitDir() // scan up the file system tree
                .build();

        git = new Git(repository);

        this.setSource(dataSource.getName());
        this.setLabel("repository " + directory.getName() + " at " + formatDate(System.currentTimeMillis()));
    }

    Git getGit() {
        return git;
    }

    @Override
    public void addAll() throws RepositoryException {
        performOperation(getGit().add()
                .addFilepattern("."));
    }

    @Override
    public void commitAll(final String message) throws RepositoryException {
        checkReadyForCommit();

        performOperation(getGit().commit()
                .setMessage(message)
                .setAllowEmpty(false)
                .setAll(true));
    }

    @Override
    public void pull() throws RepositoryException {
        checkReadyForPushOrPull();

        performOperation(getGit().pull()
                .setStrategy(MergeStrategy.RESOLVE));
    }

    @Override
    public void push() throws RepositoryException {
        checkReadyForPushOrPull();

        performOperation(getGit().push());
    }

    @Override
    public void cycle(final String message) throws RepositoryException {
        addAll();
        commitAll(message);
        pull();
        push();
    }

    @Override
    public Set<String> getAdded() throws RepositoryException {
        return wrap(() -> git.status().call().getAdded());
    }

    @Override
    public Set<String> getRemoved() throws RepositoryException {
        return wrap(() -> git.status().call().getRemoved());
    }

    @Override
    public Set<String> getUntracked() throws RepositoryException {
        return wrap(() -> git.status().call().getUntracked());
    }

    @Override
    public Set<String> getChanged() throws RepositoryException {
        return wrap(() -> git.status().call().getChanged());
    }

    @Override
    public Set<String> getModified() throws RepositoryException {
        return wrap(() -> git.status().call().getModified());
    }

    @Override
    public Set<String> getConflicting() throws RepositoryException {
        return wrap(() -> git.status().call().getConflicting());
    }

    @Override
    public Set<String> getMissing() throws RepositoryException {
        return wrap(() -> git.status().call().getMissing());
    }

    public Brain getBrain() {
        return brain;
    }

    public Note getHistory(final Limits limits) throws IOException, GitAPIException {
        long now = System.currentTimeMillis();

        String branch = repository.getBranch();
        logger.info("getting history for branch " + branch + " in " + directory.getAbsolutePath());

        NoteDTO repoNote = new NoteDTO();
        Model.setTopicId(repoNote, SemanticSynchrony.createRandomId());
        repoNote.setLabel(this.getLabel());
        repoNote.setSource(dataSource.getName());
        repoNote.setWeight(SemanticSynchrony.DEFAULT_WEIGHT);
        repoNote.setCreated(now);

        Iterable<RevCommit> log = git.log().call();
        int count = 0;
        for (RevCommit commit : log) {
            if (limits.getMaxDiffsPerRepository().isPresent() && ++count > limits.getMaxDiffsPerRepository().get())
                break;

            Note commitNote = toNote(commit, limits);
            // TODO: may be in reverse order
            repoNote.addChild(0, commitNote);
        }

        repoNote.setNumberOfChildren(ListNode.lengthOf(repoNote.getFirst()));
        return repoNote;
    }

    public void close() {
        repository.close();
    }

    private void performOperation(final GitCommand command) throws RepositoryException {
        try {
            command.call();
        } catch (GitAPIException e) {
            throw new RepositoryException(e);
        }
    }

    private <T> T wrap(final SupplierWithException<T, GitAPIException> supplier) throws RepositoryException {
        try {
            return supplier.get();
        } catch (GitAPIException e) {
            throw new RepositoryException(e);
        }
    }

    private void verifyCanRead() {
        Preconditions.checkArgument(directory.exists() && directory.canRead());
    }

    private void verifyCanWrite() {
        Preconditions.checkArgument(directory.exists() && directory.canWrite());
    }

    private boolean isMergeCommit(final RevCommit commit) {
        return commit.getFullMessage().startsWith("Merge branch");
    }

    private Note toNote(final RevCommit commit, final Limits limits) throws IOException, GitAPIException {
        NoteDTO commitNote = new NoteDTO();
        Model.setTopicId(commitNote, SemanticSynchrony.createRandomId());
        commitNote.setCreated(getTimeStamp(commit));
        commitNote.setLabel(createTitleFor(commit));
        commitNote.setWeight(SemanticSynchrony.DEFAULT_WEIGHT);
        commitNote.setSource(dataSource.getName());

        if (!isMergeCommit(commit)) {
            addDiffNotes(commitNote, commit, limits);
        }

        commitNote.setNumberOfParents(1);
        commitNote.setNumberOfChildren(countChildren(commitNote));
        return commitNote;
    }

    private int countChildren(final Note node) {
        ListNode<Note> children = node.getFirst();
        return null == children ? 0 : children.length();
    }

    private void addDiffNotes(final Note commitNote, final RevCommit commit, final Limits limits)
            throws IOException, GitAPIException {
        Optional<RevCommit> parent = getParent(commit);
        if (!parent.isPresent()) return;

        addDiffNotes(commitNote, parent.get(), commit, limits);
    }

    private void addDiffNotes(final Note commitNote, final RevCommit oldCommit, final RevCommit newCommit,
                              final Limits limits)
            throws IOException, GitAPIException {

        ObjectReader reader = repository.newObjectReader();
        CanonicalTreeParser oldTreeIter = new CanonicalTreeParser();
        oldTreeIter.reset(reader, oldCommit.getTree().getId());
        CanonicalTreeParser newTreeIter = new CanonicalTreeParser();
        newTreeIter.reset(reader, newCommit.getTree().getId());
        List<DiffEntry> diffs = git.diff()
                .setNewTree(newTreeIter)
                .setOldTree(oldTreeIter)
                .call();

        int count = 0;
        for (DiffEntry diffEntry : diffs) {
            if (limits.getMaxFilesPerDiff().isPresent() && count++ > limits.getMaxFilesPerDiff().get()) break;

            // note: ignored
            DiffEntry.ChangeType changeType = diffEntry.getChangeType();
            String oldPath = diffEntry.getOldPath();
            int score = diffEntry.getScore();

            //System.out.println("diff: " + diffEntry.toString());

            String newPath = diffEntry.getNewPath();

            String id = toId(changeType == DiffEntry.ChangeType.DELETE ? oldPath : newPath);
            Note changeNote = toTreeNode(id, getTimeStamp(newCommit), changeType);

            // TODO: may be in reverse order
            commitNote.addChild(0, changeNote);
        }
    }

    private Note toTreeNode(final String id, final long timestamp, final DiffEntry.ChangeType changeType) {
        Optional<Topic> opt = brain.getTopicGraph().getTopicById(id);

        Note note = new NoteDTO();
        if (opt.isPresent()) {
            note.setTopic(opt.get());
            note = model.view()
                    .root(note).height(0).filter(filter).style(ViewStyle.Basic.Forward.getStyle()).get();
        } else {
            //note = brain.getTopicGraph().createTopicTree(brain.getTopicGraph().createLink(null, null, null));
            Model.setTopicId(note, id);
            note.setCreated(timestamp);
            note.setLabel(titleForMissingNote(changeType));
            note.setWeight(SemanticSynchrony.DEFAULT_WEIGHT);
            note.setSource(dataSource.getName());
        }

        return note;
    }

    public static String titleForMissingNote(final DiffEntry.ChangeType changeType) {
        if (changeType == DiffEntry.ChangeType.DELETE) {
            return "[deleted]";
        } else if (changeType == DiffEntry.ChangeType.RENAME) {
            return "[renamed]";
        } else {
            return "[not available]";
        }
    }

    public static String toId(final String path) {
        String[] parts = path.split("/");
        return parts[parts.length - 1].trim();
    }

    private Optional<RevCommit> getParent(final RevCommit commit) {
        RevCommit[] parents = commit.getParents();
        return 0 == parents.length ? Optional.empty() : Optional.of(parents[0]);
    }

    private String createTitleFor(final RevCommit commit) {
        String message = commit.getFullMessage().trim();

        String dateLabel = formatDate(getTimeStamp(commit));
        String authorLabel = getPersonLabel(getAuthorOrCommitter(commit));

        return dateLabel + " " + authorLabel + ": " + message;
    }

    public static long getTimeStamp(final RevCommit commit) {
        return commit.getCommitTime() * 1000L;
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

    private void checkReadyForCommit() throws RepositoryException {
        checkNoConflicts();
        checkNoUntracked();
    }

    private void checkReadyForPushOrPull() throws RepositoryException {
        checkNoAdded();
        checkNoConflicts();
        checkNoRemoved();
        checkNoChanged();
        checkNoModified();
        checkNoUntracked();
    }

    private void checkNoAdded() throws RepositoryException {
        Preconditions.checkArgument(getAdded().isEmpty(), "added but uncommitted files present");
    }

    private void checkNoConflicts() throws RepositoryException {
        Preconditions.checkArgument(getConflicting().isEmpty(), "conflicts present");
    }

    private void checkNoRemoved() throws RepositoryException {
        Preconditions.checkArgument(getRemoved().isEmpty(), "removed but uncommitted files present");
    }

    private void checkNoChanged() throws RepositoryException {
        Preconditions.checkArgument(getChanged().isEmpty(), "changed but uncommitted files present");
    }

    private void checkNoModified() throws RepositoryException {
        Preconditions.checkArgument(getModified().isEmpty(), "modified but uncommitted files present");
    }

    private void checkNoUntracked() throws RepositoryException {
        Preconditions.checkArgument(getUntracked().isEmpty(), "untracked files present");
    }

    private interface SupplierWithException<T, E extends Exception> {
        T get() throws E;
    }

    public static class Limits {
        private Integer maxDiffsPerRepository;
        private Integer maxFilesPerDiff;

        public static Limits noLimits() {
            return new Limits();
        }

        public Optional<Integer> getMaxDiffsPerRepository() {
            return Optional.ofNullable(maxDiffsPerRepository);
        }

        public void setMaxDiffsPerRepository(int maxDiffsPerRepository) {
            this.maxDiffsPerRepository = maxDiffsPerRepository;
        }

        public Optional<Integer> getMaxFilesPerDiff() {
            return Optional.ofNullable(maxFilesPerDiff);
        }

        public void setMaxFilesPerDiff(int maxFilesPerDiff) {
            this.maxFilesPerDiff = maxFilesPerDiff;
        }
    }
}
