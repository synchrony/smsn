package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomList;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

public abstract class PGAtom extends PGGraphEntity implements Atom {

    protected PGAtom(final Vertex vertex) {
        super(vertex);
    }

    @Override
    public String getId() {
        return super.getId();
    }

    @Override
    public String getAlias() {
        return getOptionalProperty(SemanticSynchrony.ALIAS);
    }

    @Override
    public boolean setAlias(String alias) {
        return setOptionalProperty(SemanticSynchrony.ALIAS, alias);
    }

    @Override
    public Long getCreated() {
        return getRequiredProperty(SemanticSynchrony.CREATED);
    }

    @Override
    public boolean setCreated(Long created) {
        return setRequiredProperty(SemanticSynchrony.CREATED, created);
    }

    @Override
    public String getValue() {
        return (String) getOptionalProperty(SemanticSynchrony.VALUE);
    }

    @Override
    public boolean setValue(String value) {
        return setRequiredProperty(SemanticSynchrony.VALUE, value);
    }

    @Override
    public Float getPriority() {
        return getOptionalProperty(SemanticSynchrony.PRIORITY);
    }

    @Override
    public boolean setPriority(Float priority) {
        return setOptionalProperty(SemanticSynchrony.PRIORITY, priority);
    }

    @Override
    public Float getSharability() {
        return getRequiredProperty(SemanticSynchrony.SHARABILITY);
    }

    @Override
    public boolean setSharability(Float sharability) {
        return setRequiredProperty(SemanticSynchrony.SHARABILITY, sharability);
    }

    @Override
    public String getShortcut() {
        return getOptionalProperty(SemanticSynchrony.SHORTCUT);
    }

    @Override
    public boolean setShortcut(String shortcut) {
        return setOptionalProperty(SemanticSynchrony.SHORTCUT, shortcut);
    }

    @Override
    public Float getWeight() {
        return getRequiredProperty(SemanticSynchrony.WEIGHT);
    }

    @Override
    public boolean setWeight(Float weight) {
        return setRequiredProperty(SemanticSynchrony.WEIGHT, weight);
    }

    @Override
    public AtomList getNotes() {
        return asAtomList(getAtMostOneVertex(SemanticSynchrony.NOTES, Direction.OUT));
    }

    @Override
    public boolean setNotes(AtomList notes) {
        return setNotes(notes, null);
    }

    public boolean setNotes(AtomList notes, final Object edgeId) {
        boolean changed = removeNotes();
        if (null != notes) {
            addOutEdge(edgeId, ((PGGraphEntity) notes).asVertex(), SemanticSynchrony.NOTES);
        }
        return changed;
    }

    @Override
    public void forFirstOf(Consumer<AtomList> consumer) {
        forEachAdjacentVertex(SemanticSynchrony.FIRST, Direction.IN, new Consumer<Vertex>() {
            @Override
            public void accept(Vertex vertex) {
                consumer.accept(asAtomList(vertex));
            }
        });
    }

    @Override
    public void addChildAt(final Atom child, int position) {
        // create a list node for the atom and insert it
        AtomList list = createList();
        list.setFirst(child);
        if (0 == position) {
            list.setRest(getNotes());
            setNotes(list);
        } else {
            AtomList prev = getNotes();
            for (int i = 1; i < position; i++) {
                prev = prev.getRest();
            }

            list.setRest(prev.getRest());
            prev.setRest(list);
        }
    }

    @Override
    public void deleteChildAt(int position) {
        AtomList list = getNotes();

        // remove the atom's list node
        if (0 == position) {
            setNotes(list.getRest());

            deleteListNode(list);
        } else {
            AtomList prev = list;
            for (int i = 1; i < position; i++) {
                prev = prev.getRest();
            }

            AtomList l = prev.getRest();
            prev.setRest(l.getRest());
            deleteListNode(l);
        }
    }

    @Override
    public Collection<AtomList> getFirstOf() {
        List<AtomList> result = new LinkedList<>();
        forAllVertices(SemanticSynchrony.FIRST, Direction.IN, result::add);

        return result;
    }

    private void deleteListNode(final AtomList l) {
        ((PGGraphEntity) l).asVertex().remove();
    }

    private boolean removeNotes() {
        return removeEdge(SemanticSynchrony.NOTES, Direction.OUT);
    }
}
