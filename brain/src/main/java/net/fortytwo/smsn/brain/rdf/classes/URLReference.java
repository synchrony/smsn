package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleNoteClass;

import java.util.regex.Pattern;

public class URLReference extends SimpleNoteClass {

    public URLReference() {
        super(
                "url",
                Pattern.compile("http(s)?://.+"),
                null,
                null
                );
    }
}
