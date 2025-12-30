package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.model.entities.Note;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.rio.RDFHandlerException;

import java.util.regex.Pattern;

public abstract class NoteCollection extends NoteClass {

    public NoteCollection(final String name,
                          final Pattern valueRegex,
                          final Pattern aliasRegex,
                          final NoteReqex memberRegex) {
        super(name, valueRegex, aliasRegex, memberRegex);
    }

    @Override
    protected boolean isCollectionClass() {
        return true;
    }

    public IRI toRDF(final Note a,
                     final RDFizationContext context) throws RDFHandlerException {
        // Collections do not have URIs and do not have associated RDF statements.
        // Instead, statements are distributed over the members of the collection.
        return null;
    }
}
