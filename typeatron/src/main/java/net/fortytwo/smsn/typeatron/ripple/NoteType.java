package net.fortytwo.smsn.typeatron.ripple;

import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.io.RipplePrintStream;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.StackMapping;
import net.fortytwo.ripple.model.types.SimpleType;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.SimpleValueFactory;

public class NoteType extends SimpleType<Note> {

    private static final ValueFactory valueFactory = SimpleValueFactory.getInstance();

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
        // This has the effect of mapping to an externally-defined resource if the given note has an alias,
        // otherwise mapping to a reference to a thing described in the personal knowledge base.
        // In the latter case, the resource will only be accessible in an interactive setting if the thing is a public,
        // classified note and the knowledge base has been appropriately published as Linked Data.
        return null == instance.getAlias()
                ? null == Model.getTopicId(instance) ? null : valueFactory.createIRI(
                        SemanticSynchrony.iriForId(Model.getTopicId(instance)))
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
        return null == Model.getTopicId(o1)
                ? null == Model.getTopicId(o2) ? 0 : -1
                : null == Model.getTopicId(o2) ? 1 : Model.getTopicId(o1).compareTo(Model.getTopicId(o2));
    }
}
