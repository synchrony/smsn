package net.fortytwo.myotherbrain;

import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.Relation;
import com.tinkerpop.frames.VertexFrame;

/**
 * User: josh
 * Date: 5/5/11
 * Time: 2:57 PM
 */
public interface Atom extends VertexFrame {

    @Property(MyOtherBrain.CREATED)
    long getCreated();

    @Property(MyOtherBrain.CREATED)
    void setCreated(long created);

    @Property(MyOtherBrain.KEY)
    String getKey();

    @Property(MyOtherBrain.KEY)
    void setKey(String key);

    @Property(MyOtherBrain.TYPE)
    String getType();

    @Property(MyOtherBrain.TYPE)
    void setType(final String tags);

    @Property(MyOtherBrain.VALUE)
    String getValue();

    @Property(MyOtherBrain.VALUE)
    void setValue(String description);

    @Property(MyOtherBrain.SHARABILITY)
    Float getSharability();

    @Property(MyOtherBrain.SHARABILITY)
    void setSharability(float sharability);

    @Property(MyOtherBrain.WEIGHT)
    Float getWeight();

    @Property(MyOtherBrain.WEIGHT)
    void setWeight(float weight);

    @Relation(label = MyOtherBrain.FROM)
    Atom getFrom();

    @Relation(label = MyOtherBrain.FROM)
    void setFrom(Atom from);

    @Relation(label = MyOtherBrain.TO)
    Atom getTo();

    @Relation(label = MyOtherBrain.TO)
    void setTo(Atom to);
}
