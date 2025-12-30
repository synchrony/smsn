package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

/**
 * The FOAF Vocabulary as a collection of terms
 * Note: this class is used instead of org.eclipse.rdf4j.model.vocabulary.FOAF because the latter covers only
 * a small subset of the vocabulary.
 */
public interface FOAF {
    String NAMESPACE = "http://xmlns.com/foaf/0.1/";

    // TODO: add the rest of the classes
    IRI
            ACCOUNTNAME = RDF4JUtil.createIRI(NAMESPACE + "accountName"),
            ACCOUNTSERVICEHOMEPAGE = RDF4JUtil.createIRI(NAMESPACE + "accountServiceHomepage"),
            AGE = RDF4JUtil.createIRI(NAMESPACE + "age"),
            AGENT = RDF4JUtil.createIRI(NAMESPACE + "Agent"),
            AIMCHATID = RDF4JUtil.createIRI(NAMESPACE + "aimChatID"),
            BASEDNEAR = RDF4JUtil.createIRI(NAMESPACE + "based_near"),
            BIRTHDAY = RDF4JUtil.createIRI(NAMESPACE + "birthday"),
            CURRENTPROJECT = RDF4JUtil.createIRI(NAMESPACE + "currentProject"),
            DEPICTION = RDF4JUtil.createIRI(NAMESPACE + "depiction"),
            DEPICTS = RDF4JUtil.createIRI(NAMESPACE + "depicts"),
            DNACHECKSUM = RDF4JUtil.createIRI(NAMESPACE + "dnaChecksum"),
            DOCUMENT = RDF4JUtil.createIRI(NAMESPACE + "Document"),
            FAMILYNAME = RDF4JUtil.createIRI(NAMESPACE + "family_name"),
            FIRSTNAME = RDF4JUtil.createIRI(NAMESPACE + "firstName"),
            FUNDEDBY = RDF4JUtil.createIRI(NAMESPACE + "fundedBy"),
            GEEKCODE = RDF4JUtil.createIRI(NAMESPACE + "geekcode"),
            GENDER = RDF4JUtil.createIRI(NAMESPACE + "gender"),
            GIVENNAME = RDF4JUtil.createIRI(NAMESPACE + "givenname"),
            GROUP = RDF4JUtil.createIRI(NAMESPACE + "Group"),
            HOLDSACCOUNT = RDF4JUtil.createIRI(NAMESPACE + "holdsAccount"),
            HOMEPAGE = RDF4JUtil.createIRI(NAMESPACE + "homepage"),
            ICQCHATID = RDF4JUtil.createIRI(NAMESPACE + "icqChatID"),
            IMAGE = RDF4JUtil.createIRI(NAMESPACE + "Image"),
            IMG = RDF4JUtil.createIRI(NAMESPACE + "img"),
            INTEREST = RDF4JUtil.createIRI(NAMESPACE + "interest"),
            ISPRIMARYTOPICOF = RDF4JUtil.createIRI(NAMESPACE + "isPrimaryTopicOf"),
            JABBERID = RDF4JUtil.createIRI(NAMESPACE + "jabberID"),
            KNOWS = RDF4JUtil.createIRI(NAMESPACE + "knows"),
            LOGO = RDF4JUtil.createIRI(NAMESPACE + "logo"),
            MADE = RDF4JUtil.createIRI(NAMESPACE + "made"),
            MAKER = RDF4JUtil.createIRI(NAMESPACE + "maker"),
            MBOX = RDF4JUtil.createIRI(NAMESPACE + "mbox"),
            MBOX_SHA1SUM = RDF4JUtil.createIRI(NAMESPACE + "mbox_sha1sum"),
            MEMBER = RDF4JUtil.createIRI(NAMESPACE + "member"),
            MSNCHATID = RDF4JUtil.createIRI(NAMESPACE + "msnChatID"),
            MYERSBRIGGS = RDF4JUtil.createIRI(NAMESPACE + "myersBriggs"),
            NAME = RDF4JUtil.createIRI(NAMESPACE + "name"),
            NICK = RDF4JUtil.createIRI(NAMESPACE + "nick"),
            ONLINEACCOUNT = RDF4JUtil.createIRI(NAMESPACE + "OnlineAccount"),
            ORGANIZATION = RDF4JUtil.createIRI(NAMESPACE + "Organization"),
            PAGE = RDF4JUtil.createIRI(NAMESPACE + "page"),
            PASTPROJECT = RDF4JUtil.createIRI(NAMESPACE + "pastProject"),
            PERSON = RDF4JUtil.createIRI(NAMESPACE + "Person"),
            PERSONALPROFILEDOCUMENT = RDF4JUtil.createIRI(NAMESPACE + "PersonalProfileDocument"),
            PHONE = RDF4JUtil.createIRI(NAMESPACE + "phone"),
            PLAN = RDF4JUtil.createIRI(NAMESPACE + "plan"),
            PRIMARYTOPIC = RDF4JUtil.createIRI(NAMESPACE + "primaryTopic"),
            PROJECT = RDF4JUtil.createIRI(NAMESPACE + "Project"),
            PUBLICATIONS = RDF4JUtil.createIRI(NAMESPACE + "publications"),
            SCHOOLHOMEPAGE = RDF4JUtil.createIRI(NAMESPACE + "schoolHomepage"),
            SHA1 = RDF4JUtil.createIRI(NAMESPACE + "sha1"),
            SPATIALTHING = RDF4JUtil.createIRI(NAMESPACE + "SpatialThing"),
            SURNAME = RDF4JUtil.createIRI(NAMESPACE + "surname"),
            THEME = RDF4JUtil.createIRI(NAMESPACE + "theme"),
            THUMBNAIL = RDF4JUtil.createIRI(NAMESPACE + "thumbnail"),
            TIPJAR = RDF4JUtil.createIRI(NAMESPACE + "tipjar"),
            TITLE = RDF4JUtil.createIRI(NAMESPACE + "title"),
            TOPIC = RDF4JUtil.createIRI(NAMESPACE + "topic"),
            TOPICINTEREST = RDF4JUtil.createIRI(NAMESPACE + "topic_interest"),
            WEBLOG = RDF4JUtil.createIRI(NAMESPACE + "weblog"),
            WORKINFOHOMEPAGE = RDF4JUtil.createIRI(NAMESPACE + "workInfoHomepage"),
            YAHOOCHATID = RDF4JUtil.createIRI(NAMESPACE + "yahooChatID");
}
