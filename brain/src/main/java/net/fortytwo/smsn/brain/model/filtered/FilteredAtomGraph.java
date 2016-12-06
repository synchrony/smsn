package net.fortytwo.smsn.brain.model.filtered;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.Filter;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

public class FilteredAtomGraph implements AtomGraph {
    private final AtomGraph baseGraph;
    private final Filter filter;

    public FilteredAtomGraph(AtomGraph baseGraph, Filter filter) {
        this.baseGraph = baseGraph;
        this.filter = filter;
    }

    private Atom wrapAtom(final Atom baseAtom) {
        return new FilteredAtom(baseAtom);
    }

    private AtomList wrapList(final AtomList baseList) {
        return new FilteredAtomList(baseList);
    }

    private List<Atom> wrapAtoms(final Collection<Atom> baseColl) {
        List<Atom> wrapped = new LinkedList<>();
        for (Atom atom : baseColl) {
            wrapped.add(wrapAtom(atom));
        }
        return wrapped;
    }

    private Iterable<Atom> wrapAtoms(final Iterable<Atom> baseIter) {
        List<Atom> wrapped = new LinkedList<>();
        for (Atom atom : baseIter) {
            wrapped.add(wrapAtom(atom));
        }
        return wrapped;
    }

    private Collection<AtomList> wrapLists(final Collection<AtomList> baseColl) {
        Collection<AtomList> wrapped = new LinkedList<>();
        for (AtomList list : baseColl) {
            wrapped.add(wrapList(list));
        }
        return wrapped;
    }

    @Override
    public Iterable<Atom> getAllAtoms() {
        return wrapAtoms(baseGraph.getAllAtoms());
    }

    @Override
    public String idOfAtom(Atom a) {
        return baseGraph.idOfAtom(((FilteredAtom) a).baseAtom);
    }

    @Override
    public String iriOfAtom(Atom a) {
        return baseGraph.iriOfAtom(((FilteredAtom) a).baseAtom);
    }

    @Override
    public Atom getAtomById(String id) {
        return wrapAtom(baseGraph.getAtomById(id));
    }

    @Override
    public Atom createAtom(Filter filter, String id) {
        return wrapAtom(baseGraph.createAtom(filter, id));
    }

    @Override
    public AtomList createAtomList(String id) {
        return wrapList(baseGraph.createAtomList(id));
    }

    @Override
    public AtomList createAtomList(Atom... elements) {
        return wrapList(baseGraph.createAtomList(elements));
    }

    @Override
    public void removeIsolatedAtoms(Filter filter) {
        baseGraph.removeIsolatedAtoms(filter);
    }

    @Override
    public void notifyOfUpdate() {
        baseGraph.notifyOfUpdate();
    }

    @Override
    public List<Atom> getAtomsByShortcut(String shortcut, Filter filter) {
        return wrapAtoms(baseGraph.getAtomsByShortcut(shortcut, filter));
    }

    @Override
    public List<Atom> getAtomsByValueQuery(String value, Filter filter) {
        return wrapAtoms(baseGraph.getAtomsByValueQuery(value, filter));
    }

    @Override
    public List<Atom> getAtomsByAcronym(String acronym, Filter filter) {
        return wrapAtoms(baseGraph.getAtomsByAcronym(acronym, filter));
    }

    @Override
    public void reindexAtom(Atom a) {
        baseGraph.reindexAtom(((FilteredAtom) a).baseAtom);
    }

    @Override
    public long getLastUpdate() {
        return baseGraph.getLastUpdate();
    }

    @Override
    public void begin() {
        baseGraph.begin();
    }

    @Override
    public void commit() {
        baseGraph.commit();
    }

    @Override
    public void rollback() {
        baseGraph.rollback();
    }

    @Override
    public AtomGraph createFilteredGraph(Filter filter) {
        return baseGraph.createFilteredGraph(filter);
    }

    public class FilteredAtom implements Atom {
        private final Atom baseAtom;

        private boolean isVisible() {
            return filter.isVisible(baseAtom);
        }

        public FilteredAtom(Atom baseAtom) {
            this.baseAtom = baseAtom;
        }

        @Override
        public String getId() {
            return baseAtom.getId();
        }

        @Override
        public String getAlias() {
            return isVisible() ? baseAtom.getAlias() : null;
        }

        @Override
        public boolean setAlias(String alias) {
            return isVisible() && baseAtom.setAlias(alias);
        }

        @Override
        public Long getCreated() {
            return isVisible() ? baseAtom.getCreated() : null;
        }

        @Override
        public boolean setCreated(Long created) {
            return isVisible() && baseAtom.setCreated(created);
        }

        @Override
        public String getValue() {
            return isVisible() ? baseAtom.getValue() : null;
        }

        @Override
        public boolean setValue(String value) {
            return isVisible() && baseAtom.setValue(value);
        }

        @Override
        public Float getPriority() {
            return isVisible() ? baseAtom.getPriority() : null;
        }

        @Override
        public boolean setPriority(Float priority) {
            return isVisible() && setPriority(priority);
        }

        @Override
        public Float getSharability() {
            return baseAtom.getSharability();
        }

        @Override
        public boolean setSharability(Float sharability) {
            return isVisible() && baseAtom.setSharability(sharability);
        }

        @Override
        public String getShortcut() {
            return isVisible() ? baseAtom.getShortcut() : null;
        }

        @Override
        public boolean setShortcut(String shortcut) {
            return isVisible() && baseAtom.setShortcut(shortcut);
        }

        @Override
        public Float getWeight() {
            return isVisible() ? baseAtom.getWeight() : null;
        }

        @Override
        public boolean setWeight(Float weight) {
            return isVisible() && baseAtom.setWeight(weight);
        }

        @Override
        public AtomList getNotes() {
            return isVisible() ? baseAtom.getNotes() : null;
        }

        @Override
        public boolean setNotes(AtomList notes) {
            return isVisible() && baseAtom.setNotes(notes);
        }

        @Override
        public void forFirstOf(Consumer<AtomList> consumer) {
            if (isVisible()) baseAtom.forFirstOf(consumer);
        }

        @Override
        public void addChildAt(Atom child, int position) {
            if (isVisible()) baseAtom.addChildAt(wrapAtom(child), position);
        }

        @Override
        public void deleteChildAt(int position) {
            if (isVisible()) baseAtom.deleteChildAt(position);
        }

        @Override
        public Collection<AtomList> getFirstOf() {
            return wrapLists(baseAtom.getFirstOf());
        }
    }

    public class FilteredAtomList implements AtomList {
        private final AtomList baseList;

        public FilteredAtomList(AtomList baseList) {
            this.baseList = baseList;
        }

        @Override
        public String getId() {
            return baseList.getId();
        }

        @Override
        public Atom getFirst() {
            return null == baseList.getFirst() ? null : wrapAtom(baseList.getFirst());
        }

        @Override
        public boolean setFirst(Atom first) {
            return baseList.setFirst(((FilteredAtom) first).baseAtom);
        }

        @Override
        public AtomList getRest() {
            return null == baseList.getRest() ? null : wrapList(baseList.getRest());
        }

        @Override
        public boolean setRest(AtomList rest) {
            return baseList.setRest(((FilteredAtomList) rest).baseList);
        }

        @Override
        public AtomList getRestOf() {
            return null == baseList.getRestOf() ? null : wrapList(baseList.getRestOf());
        }

        @Override
        public Atom getNotesOf() {
            return null == baseList.getNotesOf() ? null : wrapAtom(baseList.getNotesOf());
        }

        @Override
        public List<Atom> toJavaList() {
            return wrapAtoms(baseList.toJavaList());
        }
    }
}
