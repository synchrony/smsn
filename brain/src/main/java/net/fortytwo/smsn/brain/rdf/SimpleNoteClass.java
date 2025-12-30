package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.model.entities.Note;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.rio.RDFHandlerException;

import java.util.regex.Pattern;

public abstract class SimpleNoteClass extends NoteClass {

    public SimpleNoteClass(final String name,
                           final Pattern valueRegex,
                           final Pattern aliasRegex,
                           final NoteReqex memberRegex) {
        super(name, valueRegex, aliasRegex, memberRegex);
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    public IRI toRDF(final Note a,
                     final RDFizationContext context) throws RDFHandlerException {
        // Some classes are meaningful only as fields of other classes.
        // They are not identified by URIs, and they are not the subjects of RDF statements.
        return null;
    }
}
