package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.Iterator;
import java.util.function.Consumer;

public abstract class PGTopic extends PGEntity implements Topic {

    protected PGTopic(Vertex vertex) {
        super(vertex);
    }

    @Override
    public String getId() {
        return getRequiredProperty(SemanticSynchrony.PropertyKeys.ID);
    }

    @Override
    public void setId(final String id) {
        String noteId = null == id ? SemanticSynchrony.createRandomId() : id;
        setRequiredProperty(SemanticSynchrony.PropertyKeys.ID, noteId);
    }

    @Override
    public boolean isIsolated() {
        return !hasAdjacentVertex(SemanticSynchrony.EdgeLabels.TOPIC, Direction.IN);
    }

    @Override
    public void forEachNote(final Consumer<Note> visitor) {
        forEachAdjacentVertex(SemanticSynchrony.EdgeLabels.TOPIC, Direction.IN,
                vertex -> visitor.accept(getGraph().asNote(vertex)));
    }

    @Override
    public Iterator<Note> getNotes() {
        return getEntities(SemanticSynchrony.EdgeLabels.TOPIC, Direction.IN, vertex -> getGraph().asNote(vertex));
    }

    @Override
    public void destroy() {
        // nothing else to do; a topic owns no other entities
        destroyInternal();
    }
}
