package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * The FOAF Vocabulary as a collection of terms
 * Note: this class is used instead of org.openrdf.model.vocabulary.FOAF because the latter covers only
 * a small subset of the vocabulary.
 */
public interface FOAF {
    public static final String NAMESPACE = "http://xmlns.com/foaf/0.1/";

    // TODO: add the rest of the classes
    public static final IRI
            ACCOUNTNAME = RDFAgents.createIRI(NAMESPACE + "accountName"),
            ACCOUNTSERVICEHOMEPAGE = RDFAgents.createIRI(NAMESPACE + "accountServiceHomepage"),
            AGE = RDFAgents.createIRI(NAMESPACE + "age"),
            AGENT = RDFAgents.createIRI(NAMESPACE + "Agent"),
            AIMCHATID = RDFAgents.createIRI(NAMESPACE + "aimChatID"),
            BASEDNEAR = RDFAgents.createIRI(NAMESPACE + "based_near"),
            BIRTHDAY = RDFAgents.createIRI(NAMESPACE + "birthday"),
            CURRENTPROJECT = RDFAgents.createIRI(NAMESPACE + "currentProject"),
            DEPICTION = RDFAgents.createIRI(NAMESPACE + "depiction"),
            DEPICTS = RDFAgents.createIRI(NAMESPACE + "depicts"),
            DNACHECKSUM = RDFAgents.createIRI(NAMESPACE + "dnaChecksum"),
            DOCUMENT = RDFAgents.createIRI(NAMESPACE + "Document"),
            FAMILYNAME = RDFAgents.createIRI(NAMESPACE + "family_name"),
            FIRSTNAME = RDFAgents.createIRI(NAMESPACE + "firstName"),
            FUNDEDBY = RDFAgents.createIRI(NAMESPACE + "fundedBy"),
            GEEKCODE = RDFAgents.createIRI(NAMESPACE + "geekcode"),
            GENDER = RDFAgents.createIRI(NAMESPACE + "gender"),
            GIVENNAME = RDFAgents.createIRI(NAMESPACE + "givenname"),
            GROUP = RDFAgents.createIRI(NAMESPACE + "Group"),
            HOLDSACCOUNT = RDFAgents.createIRI(NAMESPACE + "holdsAccount"),
            HOMEPAGE = RDFAgents.createIRI(NAMESPACE + "homepage"),
            ICQCHATID = RDFAgents.createIRI(NAMESPACE + "icqChatID"),
            IMAGE = RDFAgents.createIRI(NAMESPACE + "Image"),
            IMG = RDFAgents.createIRI(NAMESPACE + "img"),
            INTEREST = RDFAgents.createIRI(NAMESPACE + "interest"),
            ISPRIMARYTOPICOF = RDFAgents.createIRI(NAMESPACE + "isPrimaryTopicOf"),
            JABBERID = RDFAgents.createIRI(NAMESPACE + "jabberID"),
            KNOWS = RDFAgents.createIRI(NAMESPACE + "knows"),
            LOGO = RDFAgents.createIRI(NAMESPACE + "logo"),
            MADE = RDFAgents.createIRI(NAMESPACE + "made"),
            MAKER = RDFAgents.createIRI(NAMESPACE + "maker"),
            MBOX = RDFAgents.createIRI(NAMESPACE + "mbox"),
            MBOX_SHA1SUM = RDFAgents.createIRI(NAMESPACE + "mbox_sha1sum"),
            MEMBER = RDFAgents.createIRI(NAMESPACE + "member"),
            MSNCHATID = RDFAgents.createIRI(NAMESPACE + "msnChatID"),
            MYERSBRIGGS = RDFAgents.createIRI(NAMESPACE + "myersBriggs"),
            NAME = RDFAgents.createIRI(NAMESPACE + "name"),
            NICK = RDFAgents.createIRI(NAMESPACE + "nick"),
            ONLINEACCOUNT = RDFAgents.createIRI(NAMESPACE + "OnlineAccount"),
            ORGANIZATION = RDFAgents.createIRI(NAMESPACE + "Organization"),
            PAGE = RDFAgents.createIRI(NAMESPACE + "page"),
            PASTPROJECT = RDFAgents.createIRI(NAMESPACE + "pastProject"),
            PERSON = RDFAgents.createIRI(NAMESPACE + "Person"),
            PERSONALPROFILEDOCUMENT = RDFAgents.createIRI(NAMESPACE + "PersonalProfileDocument"),
            PHONE = RDFAgents.createIRI(NAMESPACE + "phone"),
            PLAN = RDFAgents.createIRI(NAMESPACE + "plan"),
            PRIMARYTOPIC = RDFAgents.createIRI(NAMESPACE + "primaryTopic"),
            PROJECT = RDFAgents.createIRI(NAMESPACE + "Project"),
            PUBLICATIONS = RDFAgents.createIRI(NAMESPACE + "publications"),
            SCHOOLHOMEPAGE = RDFAgents.createIRI(NAMESPACE + "schoolHomepage"),
            SHA1 = RDFAgents.createIRI(NAMESPACE + "sha1"),
            SPATIALTHING = RDFAgents.createIRI(NAMESPACE + "SpatialThing"),
            SURNAME = RDFAgents.createIRI(NAMESPACE + "surname"),
            THEME = RDFAgents.createIRI(NAMESPACE + "theme"),
            THUMBNAIL = RDFAgents.createIRI(NAMESPACE + "thumbnail"),
            TIPJAR = RDFAgents.createIRI(NAMESPACE + "tipjar"),
            TITLE = RDFAgents.createIRI(NAMESPACE + "title"),
            TOPIC = RDFAgents.createIRI(NAMESPACE + "topic"),
            TOPICINTEREST = RDFAgents.createIRI(NAMESPACE + "topic_interest"),
            WEBLOG = RDFAgents.createIRI(NAMESPACE + "weblog"),
            WORKINFOHOMEPAGE = RDFAgents.createIRI(NAMESPACE + "workInfoHomepage"),
            YAHOOCHATID = RDFAgents.createIRI(NAMESPACE + "yahooChatID");
}
