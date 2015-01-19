package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.rdf.SimpleAtomClass;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BibtexReference extends SimpleAtomClass {

    public BibtexReference() {
        super(
                "bibtex-reference",

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