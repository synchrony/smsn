package net.fortytwo.extendo.monitron.demos;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.util.QueryEngineAdder;
import info.aduna.io.IOUtil;
import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.MonitronService;
import net.fortytwo.extendo.monitron.events.Event;
import net.fortytwo.extendo.monitron.ontologies.MonitronOntology;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.QueryParser;
import org.openrdf.query.parser.sparql.SPARQLParser;
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

    private static void runDemo(final File dir) throws Exception {
        if (null == dir) {
            throw new IllegalArgumentException();
        }

        final QueryEngine engine = new QueryEngine();
        QueryParser queryParser = new SPARQLParser();
        String baseUri = "http://example.org/base-uri/";

        for (final File f : dir.listFiles()) {

            BindingSetHandler bsh = new BindingSetHandler() {
                public void handle(final BindingSet result) {
                    StringBuilder sb = new StringBuilder("RESULT (" + f.getName() + ")\t" + System.currentTimeMillis() + "\t");

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
                ParsedQuery pq = queryParser.parseQuery(query, baseUri);

                engine.addQuery(pq.getTupleExpr(), bsh);
            } finally {
                in.close();
            }
        }

        // First add the static data...
        RDFHandler a = new QueryEngineAdder(engine);
        RDFParser p = Rio.createParser(RDFFormat.TURTLE);
        p.setRDFHandler(a);
        p.parse(MonitronOntology.class.getResourceAsStream("universe.ttl"), baseUri);
        p.parse(MonitronOntology.class.getResourceAsStream("monitron.ttl"), baseUri);

        // ...then start the stream
        EventHandler handler = new EventHandler() {
            public void handleEvent(Event e) throws EventHandlingException {
                for (Statement st : e.getDataset().getStatements()) {
                    engine.addStatement(st);
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
