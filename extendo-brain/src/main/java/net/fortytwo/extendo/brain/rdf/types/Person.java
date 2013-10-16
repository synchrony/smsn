package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import net.fortytwo.extendo.brain.rdf.Mapper;
import net.fortytwo.extendo.brain.rdf.vocab.FOAF;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Person extends BottomUpType {
    public static final Person INSTANCE = new Person();

    private Field[] fields = new Field[]{
            new Field(false, null, AKA.INSTANCE, null, new NicknameMapper()),
            new Field(false, null, WebPage.INSTANCE, null, new HomepageMapper()),
            new Field(true, Pattern.compile("some quotes by .+]"), OpenCollection.INSTANCE, null, new QuotationMapper()),
            new Field(true, Pattern.compile("[A-Z].+ was born on .+"), TimeStampedEvent.INSTANCE, null, new BirthdayMapper()),
            // TODO: when the person passed away
            // TODO: the person's contact information
            // TODO: the person's email
            // TODO: the person's mailing address
            new Field(true, Pattern.compile("[A-Z].+s family( and relations)?"), OpenCollection.INSTANCE, Person.INSTANCE, new FamilyMembersMapper()),
            new Field(true, Pattern.compile("some of [A-Z].+s friends"), OpenCollection.INSTANCE, Person.INSTANCE, new FriendsMapper()),
            new Field(true, Pattern.compile("some things [A-Z].+ like[sd]"), OpenCollection.INSTANCE, null, new InterestsMapper()),
            new Field(true, Pattern.compile("some of [A-Z].+s papers"), OpenCollection.INSTANCE, ArticleOrBook.INSTANCE, new PublicationsMapper())
            // TODO: some things liked about the person
            // TODO: some things learned about from the person
            // TODO: memories of the person
            // TODO: relationship with the person
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

    public URI translateToRDF(final Atom a,
                               final ValueFactory vf,
                               final RDFHandler handler) throws RDFHandlerException {
        URI self = translateTypeAndAlias(a, vf, handler, FOAF.PERSON);

        if (a.getSharability() > 0.5) {
            handler.handleStatement(vf.createStatement(self, FOAF.NAME, vf.createLiteral(a.getValue())));
        } else {
            handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getValue())));
        }

        return self;
    }

    private class BirthdayMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child, URI parentUri, ValueFactory vf, RDFHandler handler) throws RDFHandlerException {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class FamilyMembersMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child, URI parentUri, ValueFactory vf, RDFHandler handler) throws RDFHandlerException {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class FriendsMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child, URI parentUri, ValueFactory vf, RDFHandler handler) throws RDFHandlerException {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class HomepageMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child, URI parentUri, ValueFactory vf, RDFHandler handler) throws RDFHandlerException {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class InterestsMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child, URI parentUri, ValueFactory vf, RDFHandler handler) throws RDFHandlerException {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class NicknameMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child, URI parentUri, ValueFactory vf, RDFHandler handler) throws RDFHandlerException {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class PublicationsMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child, URI parentUri, ValueFactory vf, RDFHandler handler) throws RDFHandlerException {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class QuotationMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child, URI parentUri, ValueFactory vf, RDFHandler handler) throws RDFHandlerException {
            System.out.println("quotation!");
        }
    }
}
