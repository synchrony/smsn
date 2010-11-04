package net.fortytwo.myotherbrain;

import junit.framework.TestCase;
import net.fortytwo.myotherbrain.access.AccessManager;
import net.fortytwo.myotherbrain.access.Session;
import net.fortytwo.myotherbrain.model.MOBModel;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.model.concepts.Association;
import net.fortytwo.myotherbrain.model.concepts.Atom;
import net.fortytwo.myotherbrain.update.WriteContext;
import org.openrdf.sail.Sail;
import org.openrdf.sail.memory.MemoryStore;

/**
 * Author: josh
 * Date: Jul 24, 2009
 * Time: 2:52:34 PM
 */
public class MyOtherBrainTest extends TestCase {

    private Sail sail;
    private MOBStore store;
    private Session session;
    private MOBModel model;

    private MOBModelConnection c;
    private WriteContext wc;

    public void setUp() throws Exception {
        sail = new MemoryStore();
        sail.initialize();
        store = new MOBStore(sail);
        store.initialize();
        store.generateSeedData();

        AccessManager am = new AccessManager(store);
        am.createAccount("bob", "bobspassword", "bob@example.org");

        session = am.createSession("bob");
        model = session.getModel();

        c = model.createConnection();
        wc = new WriteContext(c);
    }

    public void tearDown() throws Exception {
        c.commit();
        c.close();

        //store.dump(System.out);
        store.shutDown();
        sail.shutDown();
    }

    private Atom associationLabel;
    private Atom doer;
    private Atom character;
    private Atom nationality;
    private Atom provenance;
    private Atom memory;
    private Atom unknownForgotten;
    private Atom meaningOfSpeech;
    private Atom tone;
    private Atom petulantDefiant;
    private Atom contradicts;
    private Atom conflictOfInterest;
    private Atom supportingEvidence;
    private Atom mocking;
    private Atom accordingTo;
    private Atom opposites;
    private Atom member;
    private Atom conditionalUpon;
    private Atom implies;
    private Atom notFeelingIt;

    public void testAll() throws Exception {
        associationLabel = item();
        associationLabel.setDescription("special value associated with the predicate of a (subject, predicate, object)" +
                " structure which is analogous to a triple in RDF");
        doer = item();
        character = item();
        character.setName("descriptive of the character of the subject, e.g. scary, endearing, etc.");
        nationality = item();
        nationality.setName("nationality");
        provenance = item();
        memory = item();
        unknownForgotten = item();
        meaningOfSpeech = item();
        tone = item();
        petulantDefiant = item();
        petulantDefiant.setName("tone of the Hungarian guy rebuking Magnus' insult of the Finnish language");
        contradicts = item();
        conflictOfInterest = item();
        supportingEvidence = item();
        mocking = item();
        accordingTo = item();
        accordingTo.setDescription("in the expressed opinion of the object, the subject is true");
        opposites = item();
        member = item();
        conditionalUpon = item();
        implies = item();
        notFeelingIt = item();

        Atom tmp;

        Atom magnus = item();
        magnus.setName("Magnus");
        Atom hungarianGuy = item();
        hungarianGuy.setName("tubby Hungarian guy with glasses (can't remember his name)");
        Atom finnishLanguage = item();
        finnishLanguage.setName("the Finnish language");
        Atom ugly = item();
        ugly.setName("ugly (of language)");
        Atom beautiful = item();
        beautiful.setName("beautiful (of language)");
        opposites(beautiful, ugly).setName("beautiful and ugyl are opposite characteristics");

        // Magnus says Finnish is ugly, Hungarian guy says it's beautiful
        Association finnishIsUgly = hasCharacter(finnishLanguage, ugly);
        finnishIsUgly.setName("Finnish is an ugly language");
        Association magnusSaysFinnishIsUgly = saidBy(finnishIsUgly, magnus);
        magnusSaysFinnishIsUgly.setName("Magnus claims that Finnish is ugly");
        Association finnishIsBeautiful = hasCharacter(finnishLanguage, beautiful);
        finnishIsBeautiful.setName("Finnish is a beautiful language");
        Association hungarianGuySaysFinnishIsBeautiful = saidBy(finnishIsBeautiful, hungarianGuy);
        hungarianGuySaysFinnishIsBeautiful.setName("the Hungarian guy claims that Finnish is beautiful");

        // Specific event of Magnus speaking
        Atom magnusSpeaks = item();
        eventInMemory(magnusSpeaks).setName("I remember this indirectly");
        assoc(assoc(magnusSpeaks, finnishIsUgly),
                meaningOfSpeech);

        // Specific event of the Hungarian guy speaking
        Atom hungarianGuySpeaks = item();
        hungarianGuySpeaks.setName("Hungarian guy says in a thick accent: \" Finnish ist eine schoene Sprache!\"");
        eventInMemory(hungarianGuySpeaks).setName("I remember this clearly, including the Hungarian's accent and tone");
        assoc(assoc(hungarianGuySpeaks, finnishIsBeautiful),
                meaningOfSpeech);
        oneAssertionContradictsTheOther(hungarianGuySpeaks, magnusSpeaks);
        theWayYouSaidIt(hungarianGuySpeaks, petulantDefiant)
                .setDescription("the way he said it reminds me of Mr. Finer: \"'he' is nominative case!\"");

        // Magnus is Swedish, Hungarian guy is Hungarian
        Atom swedishPerson = item();
        swedishPerson.setName("Swedish");
        Association magnusIsSwedish = assertNationality(magnus, swedishPerson);
        magnusIsSwedish.setName("Magnus is Swedish");
        tmp = assertProvenance(magnusIsSwedish, memory);
        tmp.setName("I have ample memories of Magnus being Swedish, speaking Swedish");
        Atom hungarianPerson = item();
        hungarianPerson.setName("Hungarian");
        Association hungarianGuyIsHungarian = assertNationality(hungarianGuy, hungarianPerson);
        forgottenWhyIKnowThis(hungarianGuyIsHungarian)
                .setName("I don't remember how I know the Hungarian guy is Hungarian");

        // Magnus' and the Hungarian guy's nationalities bias them w.r.t. their opinions of the Finnish language
        Atom magnusConflict = conflictOfInterestMakesLessPlausible(
                magnusIsSwedish, magnusSaysFinnishIsUgly);
        magnusConflict.setName("Magnus is biased against Finnish because he is Swedish");
        Atom swedishAnimosityTowardsFinland = item();
        swedishAnimosityTowardsFinland.setName("possible friendly rivalry between Sweden and Finland");
        supportedBy(magnusConflict, swedishAnimosityTowardsFinland)
                .setName("Magnus' opinion may be influenced by his Swedish attitudes towards Finland");
        Atom hungarianGuyConflict = conflictOfInterestMakesLessPlausible(
                hungarianGuyIsHungarian, hungarianGuySaysFinnishIsBeautiful);
        hungarianGuyConflict.setName("the Hungarian guy is biased towards Finnish because he is Hungarian");
        Atom hungarianSympathyTowardsFinnish = item();
        hungarianSympathyTowardsFinnish.setName("perhaps Hungarians like Finnish because it is a related language");
        supportedBy(hungarianGuyConflict, hungarianSympathyTowardsFinnish)
                .setName("the Hungarian guy's opinion may be influenced by Hungarian attitudes towards Finnish, which is related to Hungarian");

        // Magnus ridicules Finnish for its "hard" phonetic character
        Atom yksiKaksiKolme = item();
        yksiKaksiKolme.setName("\"yksi, kaksi, kolme\"");
        Atom hardCharacter = item();
        hardCharacter.setName("\"hard\" phonetic character");
        Atom yksiIsHard = hasCharacter(yksiKaksiKolme, hardCharacter);
        yksiIsHard.setName("this expression sounds \"hard\"");
        yksiKaksiKolme.setDescription("\"one, two, three\" in Finnish");
        Atom magnusYksiRemark = item();
        magnusYksiRemark.setName("Magnus says \"yksi, kaksi, kolme\" in a mocking tone");
        eventInMemory(magnusYksiRemark)
                .setName("I remember this clearly, including Magnus' voice and his mocking tone");
        theWayYouSaidIt(magnusYksiRemark, mocking)
                .setDescription("he said it mockingly (and convincingly)");
        Atom magnusSupport = supportedBy(magnusSaysFinnishIsUgly, yksiIsHard);
        magnusSupport.setDescription("Magnus illustrates his claim that Finnish is ugly with an example of the \"hard\" character of the language");
        Atom hardImpliesUgly = implication(hardCharacter, ugly);
        hardImpliesUgly.setDescription("the implication that a hard-sounding language is also ugly");
        isConditionalUpon(magnusSupport, hardImpliesUgly)
                .setDescription("Magnus' argument rests on the hard ==> ugly implication");
        imNotFeelingIt(hardImpliesUgly)
                .setDescription("this is not convincing to me");

        // TODO: cat snagging brushed nylon
        // TODO: my opinions about each of the disputants
        // TODO: Magnus' language background
        // TODO: Magnus' convincing tone
        // TODO: the Hungarian's defensive bias seems to outweigh Magnus' bias
        // TODO: I had been predisposed to like Finnish at the time of this conversation, felt surprised by Magnus' comments
        // TODO: analogies of disputant's tones with other, unrelated situations
    }

    ////////////////////////////////////

    public void testInference() {
        // Is Finnish a beautiful language?  If so, why?  If not, why not?
        
    }

    ////////////////////////////////////

    private Atom item() {
        return wc.create(Atom.class);
    }

    private Association assoc(final Atom subject,
                              final Atom object) {
        Association a = wc.create(Association.class);
        a.setSubject(subject);
        a.setObject(object);
        return a;
    }

    private Association labeledAssociation(final Atom subject,
                                           final Atom object,
                                           final Atom label) {
        Association a = assoc(subject, object);
        Association b = assoc(a, label);
        assoc(b, associationLabel);
        return a;
    }

    ////////////////////////////////////

    private Association saidBy(final Atom assertion,
                               final Atom speaker) {
        return labeledAssociation(assertion, speaker, doer);
    }

    private Association hasCharacter(final Atom subject,
                                     final Atom object) {
        return labeledAssociation(subject, object, character);
    }

    private Association assertNationality(final Atom subject,
                                          final Atom natl) {
        return labeledAssociation(subject, natl, nationality);
    }

    private Association assertProvenance(final Atom subject,
                                         final Atom prov) {
        return labeledAssociation(subject, prov, provenance);
    }

    private Association eventInMemory(final Atom event) {
        return assoc(event, memory);
    }

    private Association oneAssertionContradictsTheOther(final Atom assertion1,
                                                        final Atom assertion2) {
        return labeledAssociation(assertion1, assertion2, contradicts);
    }

    private Association theWayYouSaidIt(final Atom assertion,
                                        final Atom theTone) {
        return labeledAssociation(assertion, theTone, tone);
    }

    private Association forgottenWhyIKnowThis(final Atom fact) {
        return assertProvenance(fact, unknownForgotten);
    }

    private Association conflictOfInterestMakesLessPlausible(final Atom fact,
                                                             final Atom assertion) {
        return labeledAssociation(fact, assertion, conflictOfInterest);
    }

    private Association supportedBy(final Atom fact,
                                    final Atom evidence) {
        return labeledAssociation(fact, evidence, supportingEvidence);
    }

    private Association trueAccordingTo(final Atom opinion,
                                        final Atom opinionHolder) {
        return labeledAssociation(opinion, opinionHolder, accordingTo);
    }

    private Association isConditionalUpon(final Atom fact,
                                          final Atom condition) {
        return labeledAssociation(fact, condition, conditionalUpon);
    }

    private Association implication(final Atom premise,
                                    final Atom conclusion) {
        return labeledAssociation(premise, conclusion, implies);
    }

    ////////////////////////////////////

    private Association imNotFeelingIt(final Atom assertion) {
        return assoc(assertion, notFeelingIt);
    }

    private Atom opposites(final Atom first,
                                     final Atom second) {
        Atom statement = item();
        assoc(statement, opposites);
        labeledAssociation(statement, first, member);
        labeledAssociation(statement, second, member);
        return statement;
    }
}