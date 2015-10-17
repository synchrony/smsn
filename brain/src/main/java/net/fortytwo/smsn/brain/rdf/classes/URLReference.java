package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleAtomClass;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
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
