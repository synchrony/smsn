package net.fortytwo.myotherbrain;

import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.Relation;
import com.tinkerpop.frames.VertexFrame;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Atom extends VertexFrame {

    @Property(MyOtherBrain.CREATED)
    Long getCreated();

    @Property(MyOtherBrain.CREATED)
    void setCreated(Long created);

    @Property(MyOtherBrain.KEY)
    String getKey();

    @Property(MyOtherBrain.KEY)
    void setKey(String key);

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

    @Relation(label = MyOtherBrain.FROM)
    Atom getFrom();

    @Relation(label = MyOtherBrain.FROM)
    void setFrom(Atom from);

    @Relation(label = MyOtherBrain.TO)
    Atom getTo();

    @Relation(label = MyOtherBrain.TO)
    void setTo(Atom to);
}
