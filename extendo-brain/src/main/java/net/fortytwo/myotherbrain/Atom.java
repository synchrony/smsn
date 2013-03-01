package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.Property;
import com.tinkerpop.frames.VertexFrame;
import net.fortytwo.extendo.ExtendoBrain;
import net.fortytwo.extendo.ExtendoBrain;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Atom extends VertexFrame {

    @Property(ExtendoBrain.ALIAS)
    String getAlias();

    @Property(ExtendoBrain.ALIAS)
    void setAlias(String alias);

    @Property(ExtendoBrain.CREATED)
    Long getCreated();

    @Property(ExtendoBrain.CREATED)
    void setCreated(Long created);

    @Property(ExtendoBrain.VALUE)
    String getValue();

    @Property(ExtendoBrain.VALUE)
    void setValue(String description);

    @Property(ExtendoBrain.SHARABILITY)
    Float getSharability();

    @Property(ExtendoBrain.SHARABILITY)
    void setSharability(Float sharability);

    @Property(ExtendoBrain.WEIGHT)
    Float getWeight();

    @Property(ExtendoBrain.WEIGHT)
    void setWeight(Float weight);

    @Adjacency(label = ExtendoBrain.NOTES)
    AtomList getNotes();

    @Adjacency(label = ExtendoBrain.NOTES)
    void setNotes(AtomList notes);

    @Adjacency(label = ExtendoBrain.FIRST, direction = Direction.IN)
    Iterable<AtomList> getFirstOf();
}
