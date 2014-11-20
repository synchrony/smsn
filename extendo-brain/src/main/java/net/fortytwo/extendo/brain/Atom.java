package net.fortytwo.extendo.brain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.VertexFrame;
import net.fortytwo.extendo.Extendo;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Atom extends VertexFrame {

    @Property(Extendo.ALIAS)
    String getAlias();

    @Property(Extendo.ALIAS)
    void setAlias(String alias);

    @Property(Extendo.CREATED)
    Long getCreated();

    @Property(Extendo.CREATED)
    void setCreated(Long created);

    @Property(Extendo.VALUE)
    String getValue();

    @Property(Extendo.VALUE)
    void setValue(String description);

    @Property(Extendo.PRIORITY)
    Float getPriority();

    @Property(Extendo.PRIORITY)
    void setPriority(Float priority);

    @Property(Extendo.SHARABILITY)
    Float getSharability();

    @Property(Extendo.SHARABILITY)
    void setSharability(Float sharability);

    @Property(Extendo.SHORTCUT)
    String getShortcut();

    @Property(Extendo.SHORTCUT)
    void setShortcut(String shortcut);

    @Property(Extendo.WEIGHT)
    Float getWeight();

    @Property(Extendo.WEIGHT)
    void setWeight(Float weight);

    @Adjacency(label = Extendo.NOTES)
    AtomList getNotes();

    @Adjacency(label = Extendo.NOTES)
    void setNotes(AtomList notes);

    @Adjacency(label = Extendo.FIRST, direction = Direction.IN)
    Iterable<AtomList> getFirstOf();
}
