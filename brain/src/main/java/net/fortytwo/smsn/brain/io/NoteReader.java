package net.fortytwo.smsn.brain.io;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

public abstract class NoteReader {

    public abstract Note parse(final InputStream inputStream) throws IOException;

    public Note parse(final String s) throws IOException {
        try (InputStream in = new ByteArrayInputStream(s.getBytes(SemanticSynchrony.UTF8))) {
            return parse(in);
        }
    }
}
