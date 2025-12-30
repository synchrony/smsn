package net.fortytwo.smsn.rdf;

import org.eclipse.rdf4j.model.Statement;

import java.util.Collection;
import java.util.Collections;

/**
 * A simple RDF dataset consisting of a collection of statements.
 * This replaces the rdfagents Dataset class for RDF4J compatibility.
 */
public class RDFDataset {
    private final Collection<Statement> statements;

    public RDFDataset(Collection<Statement> statements) {
        this.statements = statements;
    }

    public Collection<Statement> getStatements() {
        return Collections.unmodifiableCollection(statements);
    }

    public int size() {
        return statements.size();
    }

    public boolean isEmpty() {
        return statements.isEmpty();
    }
}
