package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleAtomClass;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BibtexReference extends SimpleAtomClass {

    public BibtexReference() {
        super(
                "bibtex-reference",
                // note: currently case-sensitive
                // note: . "may or may not match line terminators", hence the (.|[\n\r])
                Pattern.compile("[ \\t]*@" +
                        "(article|book|booklet|conference|inbook|incollection|inproceedings|manual|mastersthesis" +
                        "|misc|phdthesis|proceedings|techreport|unpublished)" +
                        "[ \\t]*\\{(.|[\n\r])*\\}[ \\t]*"),
                null,
                null
        );
    }
}
