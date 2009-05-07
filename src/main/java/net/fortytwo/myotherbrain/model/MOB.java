package net.fortytwo.myotherbrain.model;

import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:04:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class MOB {
    public static final String NAMESPACE = "http://fortytwo.net/2009/01/myotherbrain#";

    public static final URI
            // Classes
            ACCOUNT = getValueFactory().createURI(NAMESPACE + "Account"),
            ASSOCIATION = getValueFactory().createURI(NAMESPACE + "Association"),
            EVENT = getValueFactory().createURI(NAMESPACE + "Event"),
            FIRSTCLASSITEM = getValueFactory().createURI(NAMESPACE + "FirstClassItem"),
            GRAPH = getValueFactory().createURI(NAMESPACE + "Graph"),
            LITERAL = getValueFactory().createURI(NAMESPACE + "Literal"),
            MARKER = getValueFactory().createURI(NAMESPACE + "Marker"),
            RESOURCEALIAS = getValueFactory().createURI(NAMESPACE + "ResourceAlias"),
            SENSITIVITYLEVEL = getValueFactory().createURI(NAMESPACE + "SensitivityLevel"),
            WEBRESOURCEALIAS = getValueFactory().createURI(NAMESPACE + "WebResourceAlias"),

            // Object properties
            ICON = getValueFactory().createURI(NAMESPACE + "icon"),
            MARKEDWITH = getValueFactory().createURI(NAMESPACE + "markedWith"),
            OBJECT = getValueFactory().createURI(NAMESPACE + "object"),
            PRIVATEGRAPH = getValueFactory().createURI(NAMESPACE + "privateGraph"),
            SENSITIVITY = getValueFactory().createURI(NAMESPACE + "sensitivity"),
            SUBJECT = getValueFactory().createURI(NAMESPACE + "subject"),

            // Datatype properties
            CONTACTEMAILADDRESS = getValueFactory().createURI(NAMESPACE + "contactEmailAddress"),
            CREATIONTIME = getValueFactory().createURI(NAMESPACE + "creationTime"),
            DATATYPEURI = getValueFactory().createURI(NAMESPACE + "datatypeURI"),
            DESCRIPTION = getValueFactory().createURI(NAMESPACE + "description"),
            EMPHASIS = getValueFactory().createURI(NAMESPACE + "emphasis"),
            LANGUAGETAG = getValueFactory().createURI(NAMESPACE + "languageTag"),
            LASTMODIFICATIONTIME = getValueFactory().createURI(NAMESPACE + "lastModificationTime"),
            LEXICALFORM = getValueFactory().createURI(NAMESPACE + "lexicalForm"),
            NAME = getValueFactory().createURI(NAMESPACE + "name"),
            PASSWORDSHA1SUM = getValueFactory().createURI(NAMESPACE + "passwordSha1Sum"),
            PLACESTAMP = getValueFactory().createURI(NAMESPACE + "placeStamp"),
            REPRESENTATIONMEDIATYPE = getValueFactory().createURI(NAMESPACE + "representationMediaType"),
            REPRESENTATIONSHA1SUM = getValueFactory().createURI(NAMESPACE + "representationSha1Sum"),
            RESOURCEURI = getValueFactory().createURI(NAMESPACE + "resourceURI"),
            SCORE = getValueFactory().createURI(NAMESPACE + "score"),
            TIMESTAMP = getValueFactory().createURI(NAMESPACE + "timeStamp"),
            USERNAME = getValueFactory().createURI(NAMESPACE + "userName");

    private static ValueFactory valueFactory;

    private static ValueFactory getValueFactory() {
        if (null == valueFactory) {
            valueFactory = ValueFactoryImpl.getInstance();
        }

        return valueFactory;
    }
}
