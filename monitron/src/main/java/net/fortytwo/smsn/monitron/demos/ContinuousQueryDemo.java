package net.fortytwo.smsn.monitron.demos;

import org.eclipse.rdf4j.common.io.IOUtil;
import net.fortytwo.smsn.monitron.EventHandler;
import net.fortytwo.smsn.monitron.MonitronService;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.stream.sparql.SparqlStreamProcessor;
import net.fortytwo.stream.sparql.impl.shj.SHJSparqlStreamProcessor;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.RDFHandler;
import org.eclipse.rdf4j.rio.RDFParser;
import org.eclipse.rdf4j.rio.Rio;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.function.BiConsumer;

public class ContinuousQueryDemo {
    private static final int TUPLE_TTL = 0, QUERY_TTL = 0;

    private static void runDemo(final File dir) throws Exception {
        if (null == dir) {
            throw new IllegalArgumentException();
        }

        final SparqlStreamProcessor streamProcessor = new SHJSparqlStreamProcessor();
        String baseUri = "http://example.org/base-uri/";

        for (final File f : dir.listFiles()) {

            BiConsumer<BindingSet, Long> bsh = (result, expirationTime) -> {
                StringBuilder sb = new StringBuilder("RESULT (" + f.getName() + ")\t"
                        + System.currentTimeMillis() + "\t" + "\t" + expirationTime);

                boolean first = true;
                for (String n : result.getBindingNames()) {
                    if (first) {
                        first = false;
                    } else {
                        sb.append(", ");
                    }
                    sb.append(n).append(":").append(result.getValue(n));
                }

                System.out.println(sb);
            };

            System.out.println("RUN\t" + System.currentTimeMillis() + "\tadding query file " + f);
            try (InputStream in = new FileInputStream(f)) {
                String query = IOUtil.readString(in);

                streamProcessor.addQuery(QUERY_TTL, query, bsh);
            }
        }

        // First add the static data...
        RDFHandler a = streamProcessor.createRDFHandler(TUPLE_TTL);
        RDFParser p = Rio.createParser(RDFFormat.TURTLE);
        p.setRDFHandler(a);
        p.parse(MonitronOntology.class.getResourceAsStream("universe.ttl"), baseUri);
        p.parse(MonitronOntology.class.getResourceAsStream("monitron.ttl"), baseUri);

        // ...then start the stream
        EventHandler handler = e -> {
            for (Statement st : e.toRDF().getStatements()) {
                try {
                    streamProcessor.addInputs(TUPLE_TTL, st);
                } catch (Throwable t) {
                    throw new EventHandler.EventHandlingException(t);
                }
            }
        };
        MonitronService s = new MonitronService(System.in, handler);
        s.run();
    }

    public static void main(final String[] args) throws ParseException {
        Options options = new Options();

        Option fileOpt = new Option("d", "dir", true, "a directory full of SPARQL queries");
        fileOpt.setRequired(true);
        options.addOption(fileOpt);

        CommandLine cmd = new PosixParser().parse(options, args);
        String fileName = cmd.getOptionValue("d");

        try {
            File dir = new File(fileName);
            runDemo(dir);
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
