package net.fortytwo.myotherbrain.model.frames;

import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.Relation;
import com.tinkerpop.frames.VertexFrame;
import net.fortytwo.myotherbrain.MyOtherBrain;

import java.util.Collection;

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

    @Relation(label = "icon")
    Collection<WebResource> getIcon();

    @Relation(label = "icon")
    void addIcon(WebResource icon);

    @Relation(label = "alias")
    Collection<WebResource> getAlias();

    @Relation(label = "alias")
    void addAlias(WebResource alias);

    @Relation(label = MyOtherBrain.MEMBER)
    Collection<Atom> getMembers();

    @Relation(label = MyOtherBrain.MEMBER)
    void addMember(Atom member);
}
