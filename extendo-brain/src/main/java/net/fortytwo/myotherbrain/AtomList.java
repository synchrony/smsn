package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.VertexFrame;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface AtomList extends VertexFrame {

    @Adjacency(label = MyOtherBrain.FIRST)
    Atom getFirst();

    @Adjacency(label = MyOtherBrain.FIRST)
    void setFirst(Atom first);

    @Adjacency(label = MyOtherBrain.REST)
    AtomList getRest();

    @Adjacency(label = MyOtherBrain.REST)
    void setRest(AtomList rest);

    @Adjacency(label = MyOtherBrain.REST, direction = Direction.IN)
    AtomList getRestOf();

    @Adjacency(label = MyOtherBrain.NOTES, direction = Direction.IN)
    Atom getNotesOf();
}
