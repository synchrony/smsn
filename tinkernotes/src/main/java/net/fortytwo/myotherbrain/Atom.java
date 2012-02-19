package net.fortytwo.myotherbrain;

import com.tinkerpop.frames.Direction;
import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.Relation;
import com.tinkerpop.frames.VertexFrame;

import java.util.Collection;

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

    @Relation(label = MyOtherBrain.NOTE)
    Collection<Atom> getOutNotes();

    @Relation(label = MyOtherBrain.NOTE)
    void addOutNote(Atom other);

    @Relation(label = MyOtherBrain.NOTE)
    void removeOutNote(Atom other);

    @Relation(label = MyOtherBrain.NOTE, direction = Direction.INVERSE)
    Collection<Atom> getInNotes();

    @Relation(label = MyOtherBrain.NOTE, direction = Direction.INVERSE)
    void addInNote(Atom other);

    @Relation(label = MyOtherBrain.NOTE, direction = Direction.INVERSE)
    void removeInNote(Atom other);
}
