package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

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
    public void setId(final String id) {
        String atomId = null == id ? SemanticSynchrony.createRandomId() : id;
        setRequiredProperty(SemanticSynchrony.PropertyKeys.ID_V, atomId);

        getGraph().updateIndex(this, SemanticSynchrony.PropertyKeys.ID_V);
    }

    @Override
    public String getAlias() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.ALIAS);
    }

    @Override
    public void setAlias(String alias) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.ALIAS, alias);
    }

    @Override
    public Long getCreated() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.CREATED);
    }

    @Override
    public void setCreated(Long created) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.CREATED, created);
    }

    @Override
    public String getTitle() {
        return (String) getRequiredProperty(SemanticSynchrony.PropertyKeys.TITLE);
    }

    @Override
    public void setTitle(String title) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.TITLE, title);
        getGraph().updateIndex(this, SemanticSynchrony.PropertyKeys.TITLE);

        updateAcronym();
    }

    @Override
    public String getText() {
        return (String) getOptionalProperty(SemanticSynchrony.PropertyKeys.PAGE);
    }

    @Override
    public void setText(String text) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.PAGE, text);
    }

    @Override
    public Float getPriority() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.PRIORITY);
    }

    @Override
    public void setPriority(Float priority) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
    }

    @Override
    public Float getSharability() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.SHARABILITY);
    }

    @Override
    public void setSharability(Float sharability) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.SHARABILITY, sharability);
    }

    @Override
    public String getShortcut() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    @Override
    public void setShortcut(String shortcut) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);

        getGraph().updateIndex(this, SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    @Override
    public Float getWeight() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.WEIGHT);
    }

    @Override
    public void setWeight(Float weight) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.WEIGHT, weight);
    }

    @Override
    public EntityList<Atom> getChildren() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.NOTES, Direction.OUT, v -> getGraph().asListOfAtoms(v));
    }

    @Override
    public void setChildren(EntityList<Atom> children) {
        removeAllChildren();

        setChildrenInternal(children);
    }

    @Override
    public String getSource() {
        return getRequiredProperty(SemanticSynchrony.PropertyKeys.SOURCE);
    }

    @Override
    public void setSource(final String source) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.SOURCE, source);
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
            list.setRest(getChildren());
            setChildrenInternal(list);
        } else {
            EntityList<Atom> prev = getChildren();
            for (int i = 1; i < position; i++) {
                prev = prev.getRest();
            }

            list.setRest(prev.getRest());
            prev.setRest(list);
        }
    }

    @Override
    public void deleteChildAt(int position) {
        EntityList<Atom> list = getChildren();

        // remove the atom's list node
        if (0 == position) {
            setChildrenInternal(list.getRest());

            deleteEntity(list);
        } else {
            EntityList<Atom> prev = list;
            for (int i = 1; i < position; i++) {
                prev = prev.getRest();
            }

            EntityList<Atom> l = prev.getRest();
            prev.setRest(l.getRest());
            deleteEntity(l);
        }
    }

    private void setChildrenInternal(EntityList<Atom> children) {
        removeEdge(SemanticSynchrony.EdgeLabels.NOTES, Direction.OUT);

        if (null != children) {
            addOutEdge(((PGEntity) children).asVertex(), SemanticSynchrony.EdgeLabels.NOTES);
        }
    }

    private void removeAllChildren() {
        EntityList<Atom> cur = getChildren();
        while (null != cur) {
            EntityList<Atom> rest = cur.getRest();
            deleteEntity(cur);
            cur = rest;
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
        PGEntity entity = (PGEntity) notes;
        return entity.getAtMostOneEntity(SemanticSynchrony.EdgeLabels.NOTES, Direction.IN,
                vertex -> getGraph().asAtom(vertex));
    }

    @Override
    public void destroy() {
        destroyInternal();
    }

    private void deleteEntity(final EntityList<Atom> l) {
        ((PGEntity) l).asVertex().remove();
    }

    private void updateAcronym() {
        Vertex vertex = asVertex();
        String value = getTitle();
        String acronym = valueToAcronym(value);

        VertexProperty<String> previousProperty = vertex.property(SemanticSynchrony.PropertyKeys.ACRONYM);
        if (null != previousProperty) {
            previousProperty.remove();
        }

        if (null != acronym) {
            vertex.property(SemanticSynchrony.PropertyKeys.ACRONYM, acronym);
            getGraph().updateIndex(this, SemanticSynchrony.PropertyKeys.ACRONYM);
        }
    }

    private String valueToAcronym(final String value) {
        // index only short, name-like values, avoiding free-form text if possible
        if (null != value && value.length() <= 100) {
            String clean = cleanForAcronym(value);
            StringBuilder acronym = new StringBuilder();
            boolean isInside = false;
            for (byte b : clean.getBytes()) {
                // TODO: support international letter characters as such
                if (b >= 'a' && b <= 'z') {
                    if (!isInside) {
                        acronym.append((char) b);
                        isInside = true;
                    }
                } else if (' ' == b) {
                    isInside = false;
                }
            }

            return acronym.toString();
        } else {
            return null;
        }
    }

    private String cleanForAcronym(final String value) {
        return value.toLowerCase().replaceAll("[-_\t\n\r]", " ").trim();
    }
}
