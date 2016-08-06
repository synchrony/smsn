package net.fortytwo.smsn.brain.io.rdf;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.sail.SailException;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RDFWriter extends BrainWriter {

    private static final List<Format> formats;
    private static final Map<Format, RDFFormat> rdfFormats;

    static {
        formats = new LinkedList<>();
        rdfFormats = new HashMap<>();

        for (RDFFormat rdfFormat : new RDFFormatLoader().loadAll()) {
            List<String> fileExtensions = rdfFormat.getFileExtensions();
            Format format = new Format(rdfFormat.getName(), fileExtensions.toArray(new String[fileExtensions.size()]));
            formats.add(format);

            rdfFormats.put(format, rdfFormat);
        }
    }

    @Override
    public List<Format> getFormats() {
        return formats;
    }

    @Override
    protected void exportInternal(Brain sourceBrain, OutputStream destStream, Format format)
            throws IOException {
        RDFFormat rdfFormat = toRDFFormat(format);

        try {
            sourceBrain.getKnowledgeBase().exportRDF(destStream, rdfFormat, requireFilter());
        } catch (SailException | RDFHandlerException e) {
            throw new IOException(e);
        }
        destStream.flush();
        destStream.close();
    }

    private RDFFormat toRDFFormat(Format format) {
        return rdfFormats.get(format);
    }
}
