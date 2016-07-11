package net.fortytwo.smsn.typeatron.ripple;

import net.fortytwo.smsn.brain.BrainGraph;
import net.fortytwo.smsn.brain.Note;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.io.RipplePrintStream;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.StackMapping;
import net.fortytwo.ripple.model.types.SimpleType;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteType extends SimpleType<Note> {

    private static final ValueFactory valueFactory = new ValueFactoryImpl();

    public NoteType() {
        super(Note.class);
    }

    @Override
    public boolean isInstance(Note instance) {
        return true;
    }

    @Override
    public Value toRDF(Note instance, ModelConnection mc) throws RippleException {
        // if the note has an alias, use that as its IRI.
        // Otherwise, use the "thing" IRI based on its ID
        // This has the effect of mapping to an externally-defined resource if the given atom has an alias,
        // otherwise mapping to a reference to a thing described in the personal knowledge base.
        // In the latter case, the resource will only be accessible in an interactive setting if the thing is a public,
        // classified atom and the knowledge base has been appropriately published as Linked Data.
        return null == instance.getAlias()
                ? null == instance.getId() ? null : valueFactory.createIRI(BrainGraph.iriForId(instance.getId()))
                : valueFactory.createIRI(instance.getAlias());
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
