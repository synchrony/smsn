package net.fortytwo.extendo.typeatron.ripple;

import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.io.RipplePrintStream;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.StackMapping;
import net.fortytwo.ripple.model.types.SimpleType;
import org.openrdf.model.Value;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteType extends SimpleType<Note> {

    public NoteType() {
        super(Note.class);
    }

    @Override
    public boolean isInstance(Note instance) {
        return true;
    }

    @Override
    public Value toRDF(Note instance, ModelConnection mc) throws RippleException {
        return null == instance.getAlias()
                ? null == instance.getId() ? null : new URIImpl(BrainGraph.uriForId(instance.getId()))
                : new URIImpl(instance.getAlias());
    }

    @Override
    public StackMapping getMapping(Note instance) {
        return null;
    }

    @Override
    public void print(Note instance, RipplePrintStream p, ModelConnection mc) throws RippleException {
        p.print(instance);
    }

    @Override
    public Category getCategory() {
        return Category.OTHER_RESOURCE;
    }

    @Override
    public int compare(Note o1, Note o2, ModelConnection mc) {
        // compare by unique id
        // note: an alternative is comparison by creation time
        return null == o1.getId()
                ? null == o2.getId() ? 0 : -1
                : null == o2.getId() ? 1 : o1.getId().compareTo(o2.getId());
    }
}
