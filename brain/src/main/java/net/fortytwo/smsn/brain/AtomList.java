package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.frames.Adjacency;
import com.tinkerpop.frames.VertexFrame;
import net.fortytwo.smsn.SemanticSynchrony;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface AtomList extends VertexFrame {

    @Adjacency(label = SemanticSynchrony.FIRST)
    Atom getFirst();

    @Adjacency(label = SemanticSynchrony.FIRST)
    void setFirst(Atom first);

    @Adjacency(label = SemanticSynchrony.REST)
    AtomList getRest();

    @Adjacency(label = SemanticSynchrony.REST)
    void setRest(AtomList rest);

    @Adjacency(label = SemanticSynchrony.REST, direction = Direction.IN)
    AtomList getRestOf();

    @Adjacency(label = SemanticSynchrony.NOTES, direction = Direction.IN)
    Atom getNotesOf();
}
