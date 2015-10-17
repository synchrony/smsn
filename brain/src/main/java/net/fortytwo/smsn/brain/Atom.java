package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.VertexFrame;
import net.fortytwo.smsn.SemanticSynchrony;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Atom extends VertexFrame {

    @Property(SemanticSynchrony.ALIAS)
    String getAlias();

    @Property(SemanticSynchrony.ALIAS)
    void setAlias(String alias);

    @Property(SemanticSynchrony.CREATED)
    Long getCreated();

    @Property(SemanticSynchrony.CREATED)
    void setCreated(Long created);

    @Property(SemanticSynchrony.VALUE)
    String getValue();

    @Property(SemanticSynchrony.VALUE)
    void setValue(String value);

    @Property(SemanticSynchrony.PRIORITY)
    Float getPriority();

    @Property(SemanticSynchrony.PRIORITY)
    void setPriority(Float priority);

    @Property(SemanticSynchrony.SHARABILITY)
    Float getSharability();

    @Property(SemanticSynchrony.SHARABILITY)
    void setSharability(Float sharability);

    @Property(SemanticSynchrony.SHORTCUT)
    String getShortcut();

    @Property(SemanticSynchrony.SHORTCUT)
    void setShortcut(String shortcut);

    @Property(SemanticSynchrony.WEIGHT)
    Float getWeight();

    @Property(SemanticSynchrony.WEIGHT)
    void setWeight(Float weight);

    @Adjacency(label = SemanticSynchrony.NOTES)
    AtomList getNotes();

    @Adjacency(label = SemanticSynchrony.NOTES)
    void setNotes(AtomList notes);

    @Adjacency(label = SemanticSynchrony.FIRST, direction = Direction.IN)
    Iterable<AtomList> getFirstOf();
}
