package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import net.fortytwo.extendo.brain.rdf.Mapper;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Person implements BottomUpType {
    public static final Person INSTANCE = new Person();

    private Field[] fields = new Field[]{
            new Field(null, AKA.INSTANCE, null, new NicknameMapper()),
            new Field(null, WebPage.INSTANCE, null, new HomepageMapper()),
            new Field(Pattern.compile(".+ was born on .+"), TimeStampedEvent.INSTANCE, null, new BirthdayMapper())
            // TODO: when the person passed away
            // TODO: the person's contact information
            // TODO: the person's email
            // TODO: the person's mailing address
            // TODO: the person's family
            // TODO: the person's friends
            // TODO: some things the person likes
            // TODO: the person's writings or research
            // TODO: some things I like about the person
            // TODO: some things I learned about from the person
            // TODO: my memories of the person
            // TODO: my relationship with the person
    };

    private Person() {
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public Field[] getFields() {
        return fields;
    }

    public Pattern getValueRegex() {
        // note: currently excludes names which begin with special characters (e.g. Chinese or certain European names)
        return Pattern.compile("[A-Z].+");
    }

    public boolean childrenRequired() {
        return false;
    }

    public boolean aliasRequired() {
        return false;
    }

    private class NicknameMapper implements Mapper {
        @Override
        public void mapToRDF(final Atom parent,
                             final Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class HomepageMapper implements Mapper {
        @Override
        public void mapToRDF(final Atom parent,
                             final Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class BirthdayMapper implements Mapper {
        @Override
        public void mapToRDF(final Atom parent,
                             final Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }
}
