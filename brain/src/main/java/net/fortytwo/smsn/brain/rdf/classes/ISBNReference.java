package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleAtomClass;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ISBNReference extends SimpleAtomClass {

    public ISBNReference() {
        super(
                "isbn",
                Pattern.compile("ISBN(-10|-13)?: .+"),
                null,
                null
                );
    }
}
