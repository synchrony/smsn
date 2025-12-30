package net.fortytwo.smsn.rdf;

import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;

import java.util.UUID;

/**
 * Utility class for RDF4J operations, replacing functionality previously provided by RDFAgents.
 */
public final class RDF4JUtil {
    private static final ValueFactory VALUE_FACTORY = SimpleValueFactory.getInstance();
    private static final String RANDOM_IRI_NAMESPACE = "urn:uuid:";

    private RDF4JUtil() {
        // Utility class
    }

    /**
     * Creates an IRI from the given string.
     * This replaces RDFAgents.createIRI().
     *
     * @param iri the IRI string
     * @return an IRI instance
     */
    public static IRI createIRI(String iri) {
        return VALUE_FACTORY.createIRI(iri);
    }

    /**
     * Creates a random IRI using a UUID.
     * This replaces DatasetFactory.randomIRI().
     *
     * @return a new random IRI
     */
    public static IRI randomIRI() {
        return VALUE_FACTORY.createIRI(RANDOM_IRI_NAMESPACE + UUID.randomUUID());
    }

    /**
     * Gets the shared ValueFactory instance.
     *
     * @return the ValueFactory
     */
    public static ValueFactory getValueFactory() {
        return VALUE_FACTORY;
    }
}
