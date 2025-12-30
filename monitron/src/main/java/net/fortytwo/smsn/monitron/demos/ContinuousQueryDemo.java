package net.fortytwo.smsn.monitron.demos;

/**
 * Continuous SPARQL query demo for monitron sensor data.
 *
 * NOTE: This demo was disabled in Dec 2024 during RDF4J migration.
 * It depended on stream42-sparql which uses the deprecated OpenRDF Sesame API.
 *
 * The original functionality:
 * - Loaded SPARQL queries from a directory
 * - Registered them with SHJSparqlStreamProcessor for continuous evaluation
 * - Streamed sensor data through the query engine
 * - Output matching results in real-time
 *
 * To restore this functionality, consider:
 * - Apache Kafka Streams for event processing
 * - RDF4J's native streaming capabilities
 * - Apache Flink or Spark Streaming
 */
public class ContinuousQueryDemo {

    public static void main(final String[] args) {
        System.err.println("ContinuousQueryDemo is not available.");
        System.err.println("SPARQL streaming was removed in Dec 2024 during RDF4J migration.");
        System.err.println("See class javadoc for alternatives.");
        System.exit(1);
    }
}
