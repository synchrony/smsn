package net.fortytwo.myotherbrain.update;

/**
 * User: josh
 * Date: Sep 30, 2010
 * Time: 1:15:50 PM
 */
public class JSONRemoting {
    public static enum WriteActionType {
        addAlias,
        addMarkerTag,
        breakAssociation,
        createAssociation,
        createAtom,
        createGeoPoint,
        createLiteral,
        createWebResource,
        removeAlias,
        removeMarkerTag,
        setDescription,
        setEmphasis,
        setIcon,
        setName,
        setSensitivity,
    }

    public static enum WriteActionParam {
        subject,
        name,
        description,
        richTextDescription,
        icon,
        sensitivity,
        emphasis,
        creationTimeStamp,
        newAlias,
        newMarkerTag,
        longitude,
        latitude,
        lexicalForm,
        datatypeURI,
        languageTag,
        targetMarkerTag,
        representationMediaType,
        representationSha1Sum,
        targetAlias,
        creationPlaceStamp,
        associationSubject,
        associationObject,
    }
}
