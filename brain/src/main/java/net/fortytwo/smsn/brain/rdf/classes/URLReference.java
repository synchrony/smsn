package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleAtomClass;

import java.util.regex.Pattern;

public class URLReference extends SimpleAtomClass {

    public URLReference() {
        super(
                "url",
                Pattern.compile("http(s)?://.+"),
                null,
                null
                );
    }
}
