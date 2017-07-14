package net.fortytwo.smsn.brain.io;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Page;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

public abstract class PageParser {

    public abstract Page parse(final InputStream inputStream) throws IOException;

    public Page parse(final String s) throws IOException {
        try (InputStream in = new ByteArrayInputStream(s.getBytes(SemanticSynchrony.UTF8))) {
            return parse(in);
        }
    }
}
