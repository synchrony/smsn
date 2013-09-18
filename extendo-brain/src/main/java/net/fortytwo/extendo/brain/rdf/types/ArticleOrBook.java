package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import net.fortytwo.extendo.brain.rdf.Mapper;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ArticleOrBook implements BottomUpType {
    public static final ArticleOrBook INSTANCE = new ArticleOrBook();

    private Field[] fields = new Field[]{
            new Field(null, ISBN.INSTANCE, null, new ISBNMapper()),
            new Field(null, RFID.INSTANCE, null, new RFIDMapper()),
            new Field(null, BibtexReference.INSTANCE, null, new BibtexReferenceMapper()),
            new Field(null, OpenCollection.INSTANCE, null, new SomeNotesFromTheDocumentMapper())};

    private ArticleOrBook() {
    }

    public Field[] getFields() {
        return fields;
    }

    public Pattern getValueRegex() {
        // note: currently excludes names which begin with special characters (e.g. Chinese or certain European names)
        return Pattern.compile("[A-Z].+");
    }

    public boolean fulfillsAdditionalConstraints(final String value) {
        return true;
    }

    public boolean aliasRequired() {
        return false;
    }

    public boolean childrenRequired() {
        return false;
    }

    private class ISBNMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class RFIDMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class BibtexReferenceMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class SomeNotesFromTheDocumentMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }
}
