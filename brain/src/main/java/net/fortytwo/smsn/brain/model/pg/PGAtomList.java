package net.fortytwo.smsn.brain.model.pg;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomList;

import java.util.LinkedList;
import java.util.List;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
abstract class PGAtomList extends PGGraphEntity implements AtomList {

    public PGAtomList(final Vertex vertex) {
        super(vertex);
    }

    @Override
    public Atom getFirst() {
        return asAtom(getExactlyOneVertex(SemanticSynchrony.FIRST, Direction.OUT));
    }

    @Override
    public boolean setFirst(Atom first) {
        boolean changed = removeFirst();
        if (null != first) {
            addOutEdge(first.asVertex(), SemanticSynchrony.FIRST);
        }
        return changed;
    }

    @Override
    public AtomList getRest() {
        return asAtomList(getAtMostOneVertex(SemanticSynchrony.REST, Direction.OUT));
    }

    @Override
    public boolean setRest(AtomList rest) {
        boolean changed = removeRest();
        if (null != rest) {
            addOutEdge(rest.asVertex(), SemanticSynchrony.REST);
        }
        return changed;
    }

    @Override
    public AtomList getRestOf() {
        return asAtomList(getAtMostOneVertex(SemanticSynchrony.REST, Direction.IN));
    }

    @Override
    public Atom getNotesOf() {
        return asAtom(getAtMostOneVertex(SemanticSynchrony.NOTES, Direction.IN));
    }

    @Override
    public List<Atom> toJavaList() {
        List<Atom> list = new LinkedList<>();
        AtomList cur = this;
        while (null != cur) {
            list.add(cur.getFirst());
            cur = cur.getRest();
        }
        return list;
    }

    private boolean removeFirst() {
        return removeEdge(SemanticSynchrony.FIRST, Direction.OUT);
    }

    private boolean removeRest() {
        return removeEdge(SemanticSynchrony.REST, Direction.OUT);
    }
}
