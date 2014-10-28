package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.rdf.SimpleAtomClass;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class URLReference extends SimpleAtomClass {
    public static final URLReference INSTANCE = new URLReference();

    public URLReference() {
        super(
                "url",
                Pattern.compile("http(s)?://.+"),
                null,
                null
                );
    }
}
