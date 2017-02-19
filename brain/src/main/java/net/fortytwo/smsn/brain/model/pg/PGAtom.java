package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

public abstract class PGAtom extends PGEntity implements Atom {

    public PGAtom(final Vertex vertex) {
        super(vertex);
    }

    @Override
    public String getId() {
        return super.getId();
    }

    @Override
    public boolean setId(final String id) {
        String atomId = null == id ? SemanticSynchrony.createRandomId() : id;
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.ID_V, atomId);
    }

    @Override
    public String getAlias() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.ALIAS);
    }

    @Override
    public boolean setAlias(String alias) {
        return setOptionalProperty(SemanticSynchrony.PropertyKeys.ALIAS, alias);
    }

    @Override
    public Long getCreated() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.CREATED);
    }

    @Override
    public boolean setCreated(Long created) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.CREATED, created);
    }

    @Override
    public String getTitle() {
        return (String) getRequiredProperty(SemanticSynchrony.PropertyKeys.TITLE);
    }

    @Override
    public boolean setTitle(String title) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.TITLE, title);
    }

    @Override
    public String getText() {
        return (String) getOptionalProperty(SemanticSynchrony.PropertyKeys.PAGE);
    }

    @Override
    public boolean setText(String text) {
        return setOptionalProperty(SemanticSynchrony.PropertyKeys.PAGE, text);
    }

    @Override
    public Float getPriority() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.PRIORITY);
    }

    @Override
    public boolean setPriority(Float priority) {
        return setOptionalProperty(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
    }

    @Override
    public Float getSharability() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.SHARABILITY, 0f);
    }

    @Override
    public boolean setSharability(Float sharability) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.SHARABILITY, sharability);
    }

    @Override
    public String getShortcut() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    @Override
    public boolean setShortcut(String shortcut) {
        return setOptionalProperty(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
    }

    @Override
    public Float getWeight() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.WEIGHT, 0f);
    }

    @Override
    public boolean setWeight(Float weight) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.WEIGHT, weight);
    }

    @Override
    public EntityList<Atom> getNotes() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.NOTES, Direction.OUT, v -> getGraph().asListOfAtoms(v));
    }

    @Override
    public boolean setNotes(EntityList<Atom> notes) {
        boolean changed = removeNotes();
        if (null != notes) {
            addOutEdge(((PGEntity) notes).asVertex(), SemanticSynchrony.EdgeLabels.NOTES);
        }
        return changed;
    }

    @Override
    public void forFirstOf(Consumer<EntityList<Atom>> consumer) {
        forEachAdjacentVertex(SemanticSynchrony.EdgeLabels.FIRST, Direction.IN,
                vertex -> consumer.accept(getGraph().asListOfAtoms(vertex)));
    }

    @Override
    public void addChildAt(final Atom child, int position) {
        // create a list node for the atom and insert it
        EntityList<Atom> list = getGraph().createListOfAtoms(child);
        if (0 == position) {
            list.setRest(getNotes());
            setNotes(list);
        } else {
            EntityList<Atom> prev = getNotes();
            for (int i = 1; i < position; i++) {
                prev = prev.getRest();
            }

            list.setRest(prev.getRest());
            prev.setRest(list);
        }
    }

    @Override
    public void deleteChildAt(int position) {
        EntityList<Atom> list = getNotes();

        // remove the atom's list node
        if (0 == position) {
            setNotes(list.getRest());

            deleteListNode(list);
        } else {
            EntityList<Atom> prev = list;
            for (int i = 1; i < position; i++) {
                prev = prev.getRest();
            }

            EntityList<Atom> l = prev.getRest();
            prev.setRest(l.getRest());
            deleteListNode(l);
        }
    }

    @Override
    public Collection<EntityList<Atom>> getFirstOf() {
        List<EntityList<Atom>> result = new java.util.LinkedList<>();
        forAllVertices(SemanticSynchrony.EdgeLabels.FIRST, Direction.IN,
                vertex -> result.add(getGraph().asListOfAtoms(vertex)));

        return result;
    }

    @Override
    public Atom getSubject(EntityList<Atom> notes) {
        return ((PGEntity) notes).getAtMostOneEntity(SemanticSynchrony.EdgeLabels.NOTES, Direction.IN, v -> getGraph().asAtom(v));
    }

    @Override
    public void destroy() {
        destroyInternal();
    }

    private void deleteListNode(final EntityList<Atom> l) {
        ((PGEntity) l).asVertex().remove();
    }

    private boolean removeNotes() {
        return removeEdge(SemanticSynchrony.EdgeLabels.NOTES, Direction.OUT);
    }
}
