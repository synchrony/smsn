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
    @Property("name")
    String getName();

    @Property("name")
    void setName(String name);

    @Property(MyOtherBrain.KEY)
    String getKey();

    @Property(MyOtherBrain.KEY)
    void setKey(String key);

    @Property(MyOtherBrain.TEXT)
    String getText();

    @Property(MyOtherBrain.TEXT)
    void setText(String description);

    @Property(MyOtherBrain.TYPE)
    String getType();

    @Property(MyOtherBrain.TYPE)
    void setType(final String tags);

    @Property(MyOtherBrain.CREATED)
    long getCreated();

    @Property(MyOtherBrain.CREATED)
    void setCreated(long created);

    @Property("lastEditTime")
    long getLastEditTime();

    @Property("lastEditTime")
    void setLastEditTime(long lastEditTime);

    @Property(MyOtherBrain.WEIGHT)
    Float getWeight();

    @Property(MyOtherBrain.WEIGHT)
    void setWeight(float weight);

    @Property("sensitivity")
    Float getSensitivity();

    @Property("sensitivity")
    void setSensitivity(float sensitivity);

    @Relation(label = MyOtherBrain.FROM)
    Atom getFrom();

    @Relation(label = MyOtherBrain.FROM)
    void setFrom(Atom from);

    @Relation(label = MyOtherBrain.TO)
    Atom getTo();

    @Relation(label = MyOtherBrain.TO)
    void setTo(Atom to);
}
