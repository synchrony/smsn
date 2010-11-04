package net.fortytwo.myotherbrain.model;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:04:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class MOB {
    public static final String NAMESPACE = "http://fortytwo.net/2009/05/myotherbrain#";

    public static final String
            // Classes
            ACCOUNT = NAMESPACE + "Account",
            ASSOCIATION = NAMESPACE + "Association",
            ATOM = NAMESPACE + "Atom",
            GEOPOINT = NAMESPACE + "GeoPoint",
            GRAPH = NAMESPACE + "Graph",
            LITERAL = NAMESPACE + "Literal",
            MARKER = NAMESPACE + "Marker",
            SENSITIVITYLEVEL = NAMESPACE + "SensitivityLevel",
            WEBRESOURCE = NAMESPACE + "WebResource",

            // Object properties
            ALIAS = NAMESPACE + "alias",
            CREATIONPLACESTAMP = NAMESPACE + "creationPlaceStamp",
            ICON = NAMESPACE + "icon",
            MARKERTAG = NAMESPACE + "newMarkerTag",
            OBJECT = NAMESPACE + "object",
            PERSONALGRAPH = NAMESPACE + "personalGraph",
            SENSITIVITY = NAMESPACE + "sensitivity",
            SUBJECT = NAMESPACE + "subject",

            // Datatype properties
            CONTACTEMAILADDRESS = NAMESPACE + "contactEmailAddress",
            CREATIONTIMESTAMP = NAMESPACE + "creationTimeStamp",
            DATATYPEURI = NAMESPACE + "datatypeURI",
            DESCRIPTION = NAMESPACE + "description",
            EMPHASIS = NAMESPACE + "emphasis",
            LANGUAGETAG = NAMESPACE + "languageTag",
            LATITUDE = NAMESPACE + "representationSha1Sum",
            LEXICALFORM = NAMESPACE + "lexicalForm",
            LONGITUDE = NAMESPACE + "representationMediaType",
            NAME = NAMESPACE + "name",
            PASSWORDSHA1SUM = NAMESPACE + "passwordSha1Sum",
            PLACESTAMP = NAMESPACE + "placeStamp",
            REPRESENTATIONMEDIATYPE = NAMESPACE + "representationMediaType",
            REPRESENTATIONSHA1SUM = NAMESPACE + "representationSha1Sum",
            RESOURCEURI = NAMESPACE + "resourceURI",
            RICHTEXTDESCRIPTION = NAMESPACE + "richTextDescription",
            SCORE = NAMESPACE + "score",
            USERNAME = NAMESPACE + "userName",

            // Special individuals
            BROKEN = NAMESPACE + "Broken",
            MOBADMINGRAPH = NAMESPACE + "MOBAdminGraph",
            MOBONTOLOGYGRAPH = NAMESPACE + "MOBOntologyGraph",
            PERSONAL = NAMESPACE + "Personal",
            PRIVATE = NAMESPACE + "Private",
            PUBLIC = NAMESPACE + "Public",
            STARRED = NAMESPACE + "Starred";
}
