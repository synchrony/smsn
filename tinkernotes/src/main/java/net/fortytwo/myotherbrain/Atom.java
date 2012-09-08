package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.VertexFrame;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Atom extends VertexFrame {

    @Property(MyOtherBrain.ALIAS)
    String getAlias();

    @Property(MyOtherBrain.ALIAS)
    void setAlias(String alias);

    @Property(MyOtherBrain.CREATED)
    Long getCreated();

    @Property(MyOtherBrain.CREATED)
    void setCreated(Long created);

    @Property(MyOtherBrain.VALUE)
    String getValue();

    @Property(MyOtherBrain.VALUE)
    void setValue(String description);

    @Property(MyOtherBrain.SHARABILITY)
    Float getSharability();

    @Property(MyOtherBrain.SHARABILITY)
    void setSharability(Float sharability);

    @Property(MyOtherBrain.WEIGHT)
    Float getWeight();

    @Property(MyOtherBrain.WEIGHT)
    void setWeight(Float weight);

    @Adjacency(label = MyOtherBrain.NOTES)
    AtomList getNotes();

    @Adjacency(label = MyOtherBrain.NOTES)
    void setNotes(AtomList notes);

    @Adjacency(label = MyOtherBrain.FIRST, direction = Direction.IN)
    AtomList getFirstOf();
}
