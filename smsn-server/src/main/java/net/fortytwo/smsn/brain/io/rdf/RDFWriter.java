package net.fortytwo.smsn.brain.io.rdf;

import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.Format;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.sail.SailException;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class RDFWriter extends NoteWriter {

    private static final List<Format> formats;
    private static final Map<Format, RDFFormat> rdfFormats;

    static {
        formats = new LinkedList<>();
        rdfFormats = new HashMap<>();

        for (RDFFormat rdfFormat : new RDFFormatLoader().loadAll()) {
            List<String> fileExtensions = rdfFormat.getFileExtensions();
            Format format = new Format(
                    rdfFormat.getName(),
                    Format.Type.FileBased,
                    fileExtensions.toArray(new String[fileExtensions.size()]));
            formats.add(format);

            rdfFormats.put(format, rdfFormat);
        }
    }

    @Override
    public List<Format> getFormats() {
        return formats;
    }

    @Override
    public void doWrite(Context context) throws IOException {

        RDFFormat rdfFormat = toRDFFormat(context.getFormat());

        try {
            context.getKnowledgeBase().exportRDF(context.getDestStream(), rdfFormat, context.getFilter());
        } catch (SailException | RDFHandlerException e) {
            throw new IOException(e);
        }
    }

    private RDFFormat toRDFFormat(Format format) {
        return rdfFormats.get(format);
    }
}
