package net.fortytwo.extendo.monitron.demos;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.impl.QueryEngineImpl;
import info.aduna.io.IOUtil;
import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.MonitronService;
import net.fortytwo.extendo.monitron.events.MonitronEvent;
import net.fortytwo.extendo.monitron.ontologies.MonitronOntology;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ContinuousQueryDemo {
    private static final int TUPLE_TTL = 0, QUERY_TTL = 0;

    private static void runDemo(final File dir) throws Exception {
        if (null == dir) {
            throw new IllegalArgumentException();
        }

        final QueryEngineImpl engine = new QueryEngineImpl();
        String baseUri = "http://example.org/base-uri/";

        for (final File f : dir.listFiles()) {

            BindingSetHandler bsh = new BindingSetHandler() {
                public void handle(final BindingSet result) {
                    StringBuilder sb = new StringBuilder("RESULT (" + f.getName() + ")\t"
                            + System.currentTimeMillis() + "\t");

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
                }
            };

            System.out.println("RUN\t" + System.currentTimeMillis() + "\tadding query file " + f);
            InputStream in = new FileInputStream(f);
            try {
                String query = IOUtil.readString(in);

                engine.addQuery(QUERY_TTL, query, bsh);
            } finally {
                in.close();
            }
        }

        // First add the static data...
        RDFHandler a = engine.createRDFHandler(TUPLE_TTL);
        RDFParser p = Rio.createParser(RDFFormat.TURTLE);
        p.setRDFHandler(a);
        p.parse(MonitronOntology.class.getResourceAsStream("universe.ttl"), baseUri);
        p.parse(MonitronOntology.class.getResourceAsStream("monitron.ttl"), baseUri);

        // ...then start the stream
        EventHandler handler = new EventHandler() {
            public void handleEvent(MonitronEvent e) throws EventHandlingException {
                for (Statement st : e.toRDF().getStatements()) {
                    try {
                        engine.addStatements(TUPLE_TTL, st);
                    } catch (Throwable t) {
                        throw new EventHandlingException(t);
                    }
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
