package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.VertexFrame;
import net.fortytwo.extendo.ExtendoBrain;
import net.fortytwo.extendo.ExtendoBrain;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface AtomList extends VertexFrame {

    @Adjacency(label = ExtendoBrain.FIRST)
    Atom getFirst();

    @Adjacency(label = ExtendoBrain.FIRST)
    void setFirst(Atom first);

    @Adjacency(label = ExtendoBrain.REST)
    AtomList getRest();

    @Adjacency(label = ExtendoBrain.REST)
    void setRest(AtomList rest);

    @Adjacency(label = ExtendoBrain.REST, direction = Direction.IN)
    AtomList getRestOf();

    @Adjacency(label = ExtendoBrain.NOTES, direction = Direction.IN)
    Atom getNotesOf();
}
