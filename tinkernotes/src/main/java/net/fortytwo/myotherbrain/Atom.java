package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.VertexFrame;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Atom extends VertexFrame {

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

    @Adjacency(label = MyOtherBrain.NOTE)
    Iterable<Atom> getOutNotes();

    @Adjacency(label = MyOtherBrain.NOTE)
    void addOutNote(Atom other);

    @Adjacency(label = MyOtherBrain.NOTE)
    void removeOutNote(Atom other);

    @Adjacency(label = MyOtherBrain.NOTE, direction = Direction.IN)
    Iterable<Atom> getInNotes();

    @Adjacency(label = MyOtherBrain.NOTE, direction = Direction.IN)
    void addInNote(Atom other);

    @Adjacency(label = MyOtherBrain.NOTE, direction = Direction.IN)
    void removeInNote(Atom other);
}
