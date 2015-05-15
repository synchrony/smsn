package net.fortytwo.extendo.rdf.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * The FOAF Vocabulary as a collection of terms
 * Note: this class is used instead of org.openrdf.model.vocabulary.FOAF because the latter covers only
 * a small subset of the vocabulary.
 *
 * @author Joshua Shinavier (http://fortytwo.net).
 */
public interface FOAF {
    public static final String NAMESPACE = "http://xmlns.com/foaf/0.1/";

    // TODO: add the rest of the classes
    public static final URI
            ACCOUNTNAME = new URIImpl(NAMESPACE + "accountName"),
            ACCOUNTSERVICEHOMEPAGE = new URIImpl(NAMESPACE + "accountServiceHomepage"),
            AGE = new URIImpl(NAMESPACE + "age"),
            AGENT = new URIImpl(NAMESPACE + "Agent"),
            AIMCHATID = new URIImpl(NAMESPACE + "aimChatID"),
            BASEDNEAR = new URIImpl(NAMESPACE + "based_near"),
            BIRTHDAY = new URIImpl(NAMESPACE + "birthday"),
            CURRENTPROJECT = new URIImpl(NAMESPACE + "currentProject"),
            DEPICTION = new URIImpl(NAMESPACE + "depiction"),
            DEPICTS = new URIImpl(NAMESPACE + "depicts"),
            DNACHECKSUM = new URIImpl(NAMESPACE + "dnaChecksum"),
            DOCUMENT = new URIImpl(NAMESPACE + "Document"),
            FAMILYNAME = new URIImpl(NAMESPACE + "family_name"),
            FIRSTNAME = new URIImpl(NAMESPACE + "firstName"),
            FUNDEDBY = new URIImpl(NAMESPACE + "fundedBy"),
            GEEKCODE = new URIImpl(NAMESPACE + "geekcode"),
            GENDER = new URIImpl(NAMESPACE + "gender"),
            GIVENNAME = new URIImpl(NAMESPACE + "givenname"),
            GROUP = new URIImpl(NAMESPACE + "Group"),
            HOLDSACCOUNT = new URIImpl(NAMESPACE + "holdsAccount"),
            HOMEPAGE = new URIImpl(NAMESPACE + "homepage"),
            ICQCHATID = new URIImpl(NAMESPACE + "icqChatID"),
            IMAGE = new URIImpl(NAMESPACE + "Image"),
            IMG = new URIImpl(NAMESPACE + "img"),
            INTEREST = new URIImpl(NAMESPACE + "interest"),
            ISPRIMARYTOPICOF = new URIImpl(NAMESPACE + "isPrimaryTopicOf"),
            JABBERID = new URIImpl(NAMESPACE + "jabberID"),
            KNOWS = new URIImpl(NAMESPACE + "knows"),
            LOGO = new URIImpl(NAMESPACE + "logo"),
            MADE = new URIImpl(NAMESPACE + "made"),
            MAKER = new URIImpl(NAMESPACE + "maker"),
            MBOX = new URIImpl(NAMESPACE + "mbox"),
            MBOX_SHA1SUM = new URIImpl(NAMESPACE + "mbox_sha1sum"),
            MEMBER = new URIImpl(NAMESPACE + "member"),
            MSNCHATID = new URIImpl(NAMESPACE + "msnChatID"),
            MYERSBRIGGS = new URIImpl(NAMESPACE + "myersBriggs"),
            NAME = new URIImpl(NAMESPACE + "name"),
            NICK = new URIImpl(NAMESPACE + "nick"),
            ONLINEACCOUNT = new URIImpl(NAMESPACE + "OnlineAccount"),
            ORGANIZATION = new URIImpl(NAMESPACE + "Organization"),
            PAGE = new URIImpl(NAMESPACE + "page"),
            PASTPROJECT = new URIImpl(NAMESPACE + "pastProject"),
            PERSON = new URIImpl(NAMESPACE + "Person"),
            PERSONALPROFILEDOCUMENT = new URIImpl(NAMESPACE + "PersonalProfileDocument"),
            PHONE = new URIImpl(NAMESPACE + "phone"),
            PLAN = new URIImpl(NAMESPACE + "plan"),
            PRIMARYTOPIC = new URIImpl(NAMESPACE + "primaryTopic"),
            PROJECT = new URIImpl(NAMESPACE + "Project"),
            PUBLICATIONS = new URIImpl(NAMESPACE + "publications"),
            SCHOOLHOMEPAGE = new URIImpl(NAMESPACE + "schoolHomepage"),
            SHA1 = new URIImpl(NAMESPACE + "sha1"),
            SPATIALTHING = new URIImpl(NAMESPACE + "SpatialThing"),
            SURNAME = new URIImpl(NAMESPACE + "surname"),
            THEME = new URIImpl(NAMESPACE + "theme"),
            THUMBNAIL = new URIImpl(NAMESPACE + "thumbnail"),
            TIPJAR = new URIImpl(NAMESPACE + "tipjar"),
            TITLE = new URIImpl(NAMESPACE + "title"),
            TOPIC = new URIImpl(NAMESPACE + "topic"),
            TOPICINTEREST = new URIImpl(NAMESPACE + "topic_interest"),
            WEBLOG = new URIImpl(NAMESPACE + "weblog"),
            WORKINFOHOMEPAGE = new URIImpl(NAMESPACE + "workInfoHomepage"),
            YAHOOCHATID = new URIImpl(NAMESPACE + "yahooChatID");
}
