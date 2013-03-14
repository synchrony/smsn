package net.fortytwo.extendo.brain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.VertexFrame;
import net.fortytwo.extendo.Extendo;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface AtomList extends VertexFrame {

    @Adjacency(label = Extendo.FIRST)
    Atom getFirst();

    @Adjacency(label = Extendo.FIRST)
    void setFirst(Atom first);

    @Adjacency(label = Extendo.REST)
    AtomList getRest();

    @Adjacency(label = Extendo.REST)
    void setRest(AtomList rest);

    @Adjacency(label = Extendo.REST, direction = Direction.IN)
    AtomList getRestOf();

    @Adjacency(label = Extendo.NOTES, direction = Direction.IN)
    Atom getNotesOf();
}
