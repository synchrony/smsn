package net.fortytwo.smsn.server.io;

import net.fortytwo.smsn.brain.BrainGraph;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Exporter {
    public void doExport(BrainGraph sourceGraph, OutputStream destStream) throws IOException {
        exportInternal(sourceGraph, destStream);
    }

    protected abstract void exportInternal(BrainGraph sourceGraph, OutputStream destStream) throws IOException;
}