package net.fortytwo.smsn.brain.repository;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.ListNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.entities.TreeNode;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * TopicGraph adapter for FileBasedAtomRepository.
 * Provides a TopicGraph interface for VCS export and other operations that need
 * to iterate over all notes.
 */
public class FileBasedTopicGraph implements TopicGraph {

    private final FileBasedAtomRepository repository;

    public FileBasedTopicGraph(FileBasedAtomRepository repository) {
        this.repository = repository;
    }

    @Override
    public Iterable<Note> getAllNotes() {
        return () -> {
            List<AtomId> allIds = repository.getAllAtomIds();
            Iterator<AtomId> idIterator = allIds.iterator();

            return new Iterator<Note>() {
                @Override
                public boolean hasNext() {
                    return idIterator.hasNext();
                }

                @Override
                public Note next() {
                    AtomId id = idIterator.next();
                    Optional<Atom> atomOpt = repository.findById(id);
                    if (atomOpt.isPresent()) {
                        return atomToNote(atomOpt.get());
                    }
                    return null;
                }
            };
        };
    }

    @Override
    public Optional<Note> getNoteById(AtomId id) {
        return repository.findById(id).map(this::atomToNote);
    }

    @Override
    public List<Note> getNotesByAcronym(String acronym, Filter filter) {
        return repository.findByAcronym(acronym, filter).stream()
                .map(this::atomToNote)
                .toList();
    }

    @Override
    public List<Note> getNotesByShortcut(String shortcut, Filter filter) {
        return repository.findByShortcut(shortcut, filter).stream()
                .map(this::atomToNote)
                .toList();
    }

    @Override
    public List<Note> getNotesByTitleQuery(String value, Filter filter) {
        return repository.search(value, filter).stream()
                .map(this::atomToNote)
                .toList();
    }

    @Override
    public AtomId idOf(Note a) {
        return Note.getId(a);
    }

    @Override
    public String iriOf(Note a) {
        return "urn:smsn:" + Note.getId(a).value;
    }

    /**
     * Convert an Atom to a Note for export/compatibility.
     */
    private Note atomToNote(Atom atom) {
        return new AtomNote(atom, repository);
    }

    // ========== Unsupported creation operations ==========
    // These are not needed for read-only operations like VCS export

    @Override
    public Topic createTopic(AtomId id) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public Page createPage(Link root) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public Link createLink(Topic target, String label, Role role) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public TreeNode<Link> createTopicTree(Link link) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public Note createNote(AtomId id) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public Note createNoteWithProperties(Filter filter, AtomId id) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public ListNode<Link> toList(Link... elements) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public ListNode<Topic> toList(Topic... elements) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public ListNode<TreeNode<Link>> toList(TreeNode<Link>... elements) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public ListNode<Note> createListOfNotes(Note... elements) {
        throw new UnsupportedOperationException("FileBasedTopicGraph is read-only for VCS export");
    }

    @Override
    public void removeIsolatedNotes(Filter filter) {
        repository.removeIsolatedAtoms(filter);
    }

    @Override
    public void notifyOfUpdate() {
        repository.notifyOfUpdate();
    }

    @Override
    public void reindex(Note a) {
        // No-op for file-based - indexing happens on save
    }

    @Override
    public long getLastUpdate() {
        return repository.getLastUpdate();
    }

    @Override
    public void begin() {
        repository.begin();
    }

    @Override
    public void commit() {
        repository.commit();
    }

    @Override
    public void rollback() {
        repository.rollback();
    }

    @Override
    public TopicGraph createFilteredGraph(Filter filter) {
        // Return self with filter applied - not fully implemented
        return this;
    }

    /**
     * Note implementation that wraps an Atom for read-only access.
     */
    private static class AtomNote implements Note {
        private final Atom atom;
        private final FileBasedAtomRepository repository;
        private ListNode<Note> children;

        AtomNote(Atom atom, FileBasedAtomRepository repository) {
            this.atom = atom;
            this.repository = repository;
        }

        @Override
        public void destroy() {
            throw new UnsupportedOperationException("AtomNote is read-only");
        }

        @Override
        @SuppressWarnings("unchecked")
        public <V> V optProperty(String key) {
            return getProperty(key);
        }

        @Override
        @SuppressWarnings("unchecked")
        public <V> V getProperty(String key) {
            switch (key) {
                case SemanticSynchrony.PropertyKeys.ID:
                    return (V) atom.id.value;
                case SemanticSynchrony.PropertyKeys.TITLE:
                    return (V) atom.title;
                case SemanticSynchrony.PropertyKeys.SOURCE:
                    return (V) atom.source.value;
                case SemanticSynchrony.PropertyKeys.CREATED:
                    return (V) atom.created.value;
                case SemanticSynchrony.PropertyKeys.WEIGHT:
                    return (V) atom.weight.value;
                case SemanticSynchrony.PropertyKeys.PRIORITY:
                    return atom.priority.isPresent() ? (V) atom.priority.get().value : null;
                case SemanticSynchrony.PropertyKeys.SHORTCUT:
                    return atom.shortcut.isPresent() ? (V) atom.shortcut.get() : null;
                case SemanticSynchrony.PropertyKeys.ALIAS:
                    return atom.alias.isPresent() ? (V) atom.alias.get() : null;
                case SemanticSynchrony.PropertyKeys.TEXT:
                    return atom.text.isPresent() ? (V) atom.text.get() : null;
                case SemanticSynchrony.PropertyKeys.ROLE:
                    return (V) Role.Entity;
                default:
                    return null;
            }
        }

        @Override
        public <V> void setProperty(String key, V value) {
            throw new UnsupportedOperationException("AtomNote is read-only");
        }

        @Override
        public Topic getTopic() {
            return null;
        }

        @Override
        public void setTopic(Topic topic) {
            throw new UnsupportedOperationException("AtomNote is read-only");
        }

        @Override
        public ListNode<Note> getChildren() {
            if (children == null && !atom.children.isEmpty()) {
                List<Note> childNotes = new ArrayList<>();
                for (AtomId childId : atom.children) {
                    repository.findById(childId).ifPresent(childAtom ->
                            childNotes.add(new AtomNote(childAtom, repository)));
                }
                if (!childNotes.isEmpty()) {
                    children = ListNodeDTO.fromArray(childNotes.toArray(new Note[0]));
                }
            }
            return children;
        }

        @Override
        public void setChildren(ListNode<Note> children) {
            throw new UnsupportedOperationException("AtomNote is read-only");
        }

        @Override
        public void forFirstOf(Consumer<ListNode<Note>> consumer) {
            // No-op for read-only
        }

        @Override
        public void addChildAt(Note child, int position) {
            throw new UnsupportedOperationException("AtomNote is read-only");
        }

        @Override
        public void deleteChildAt(int position) {
            throw new UnsupportedOperationException("AtomNote is read-only");
        }

        @Override
        public Collection<ListNode<Note>> getFirstOf() {
            return Collections.emptyList();
        }

        @Override
        public Note getSubject(ListNode<Note> notes) {
            return null;
        }
    }
}
