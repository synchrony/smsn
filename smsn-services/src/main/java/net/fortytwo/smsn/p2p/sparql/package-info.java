/**
 * SPARQL streaming package - DEPRECATED.
 *
 * This package previously contained:
 * - ProxySparqlStreamProcessor: Proxy for SPARQL stream processing
 * - QueryEngineWrapper: Wrapper for continuous query engine
 * - SimpleJSONRDFFormat: JSON serialization for RDF data
 *
 * These classes were removed in Dec 2024 during RDF4J migration because they
 * depended on stream42-sparql, which uses the deprecated OpenRDF Sesame API.
 *
 * To restore SPARQL streaming functionality, consider:
 * - Apache Kafka Streams for event processing
 * - RDF4J's native streaming capabilities
 * - gRPC or WebSockets for real-time communication
 * - Apache Jena's streaming support
 */
package net.fortytwo.smsn.p2p.sparql;
