package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import net.fortytwo.extendo.brain.rdf.Mapper;
import net.fortytwo.extendo.brain.rdf.MappingContext;
import net.fortytwo.extendo.rdf.vocab.FOAF;
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

    private Field[] fields = null;

    private Person() {
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public Field[] getFields() {
        if (null == fields) {
            fields = new Field[]{
                    new Field(false, null, AKA.INSTANCE, null, new NicknameMapper()),
                    new Field(false, null, WebPage.INSTANCE, null, new HomepageMapper()),
                    new Field(true, Pattern.compile("some of [A-Z].+s papers"), OpenCollection.INSTANCE, ArticleOrBook.INSTANCE, new PublicationsMapper()),
                    new Field(true, Pattern.compile("some quotes by [A-Z].+"), OpenCollection.INSTANCE, VocabularyTerm.INSTANCE, new QuotationMapper()),
                    new Field(true, Pattern.compile("[A-Z].+ was born on .+"), TimeStampedEvent.INSTANCE, null, new BirthdayMapper()),
                    // TODO: when the person passed away
                    // TODO: the person's contact information
                    // TODO: the person's email
                    // TODO: the person's mailing address
                    new Field(true, Pattern.compile("[A-Z].+s family( and relations)?"), OpenCollection.INSTANCE, Person.INSTANCE, new FamilyMembersMapper()),
                    new Field(true, Pattern.compile("some of [A-Z].+s friends"), OpenCollection.INSTANCE, Person.INSTANCE, new FriendsMapper()),
                    new Field(true, Pattern.compile("some things [A-Z].+ like[sd]"), OpenCollection.INSTANCE, null, new InterestsMapper()),
                    // TODO: some things liked about the person
                    // TODO: some things learned about from the person
                    // TODO: memories of the person
                    // TODO: relationship with the person
            };
        }

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
        public void mapToRDF(Atom birthdayEvent, MappingContext context) throws RDFHandlerException {
            System.out.println("birthday!");

            // technically, this is a misuse of foaf:birthday, which expects
            // string values of the form mm-dd, eg. '12-31', as opposed to
            // Extend-o-Brain's structured date values, which are represented
            // using the Event Ontology.
            // Nevertheless, foaf:birthday is used here because of its
            // appropriateness for the "birthday" meaning and its association with
            // foaf:Person, the RDF type associated with the EoB Person type.

            context.getHandler().handleStatement(
                    context.getValueFactory().createStatement(
                            context.getReferenceUri(), FOAF.BIRTHDAY, context.uriOf(birthdayEvent)));
        }
    }

    private class FamilyMembersMapper implements Mapper {
        public void mapToRDF(Atom coll, MappingContext context) throws RDFHandlerException {
            System.out.println("family!");

            // for now, family and relations other than friends are just more foaf:knows
            for (Atom fam : context.getKnowledgeBase().contentsOfCollection(coll)) {
                if (context.getKnowledgeBase().getTypeOf(fam) == Person.INSTANCE) {
                    context.getHandler().handleStatement(
                            context.getValueFactory().createStatement(
                                    context.getReferenceUri(), FOAF.KNOWS, context.uriOf(fam)));
                }
            }
        }
    }

    private class FriendsMapper implements Mapper {
        public void mapToRDF(Atom coll, MappingContext context) throws RDFHandlerException {
            System.out.println("friends!");

            for (Atom friend : context.getKnowledgeBase().contentsOfCollection(coll)) {
                if (context.getKnowledgeBase().getTypeOf(friend) == Person.INSTANCE) {
                    context.getHandler().handleStatement(
                            context.getValueFactory().createStatement(
                                    context.getReferenceUri(), FOAF.KNOWS, context.uriOf(friend)));
                }
            }
        }
    }

    private class HomepageMapper implements Mapper {
        public void mapToRDF(Atom page, MappingContext context) throws RDFHandlerException {
            context.getHandler().handleStatement(
                    context.getValueFactory().createStatement(
                            context.getReferenceUri(), FOAF.HOMEPAGE, context.uriOf(page)));
        }
    }

    private class InterestsMapper implements Mapper {
        public void mapToRDF(Atom child, MappingContext context) throws RDFHandlerException {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }

    private class NicknameMapper implements Mapper {
        public void mapToRDF(Atom child, MappingContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();

            // TODO: this is an abuse of foaf:nick even when the domain is foaf:Person as it is here...
            // foaf:nick is supposed to be used for online handles, not aliases in general
            context.getHandler().handleStatement(
                    vf.createStatement(
                            context.getReferenceUri(), FOAF.NICK, vf.createLiteral(AKA.extractAlias(child.getValue()))));
        }
    }

    private class PublicationsMapper implements Mapper {
        public void mapToRDF(Atom coll, MappingContext context) throws RDFHandlerException {
            System.out.println("publications!");

            for (Atom publication : context.getKnowledgeBase().contentsOfCollection(coll)) {
                //System.out.println("\tpub: " + publication.getValue());
                if (context.getKnowledgeBase().getTypeOf(publication) == ArticleOrBook.INSTANCE) {
                    context.getHandler().handleStatement(
                            context.getValueFactory().createStatement(
                                    context.getReferenceUri(), FOAF.MADE, context.uriOf(publication)));
                }
            }
        }
    }

    private class QuotationMapper implements Mapper {
        public void mapToRDF(Atom coll, MappingContext context) throws RDFHandlerException {
            System.out.println("quotations!");
            //...
        }
    }
}
