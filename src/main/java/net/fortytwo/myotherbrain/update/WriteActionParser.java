package net.fortytwo.myotherbrain.update;

import net.fortytwo.myotherbrain.update.actions.AddAlias;
import net.fortytwo.myotherbrain.update.actions.AddMarkerTag;
import net.fortytwo.myotherbrain.update.actions.BreakAssociation;
import net.fortytwo.myotherbrain.update.actions.CreateAssociation;
import net.fortytwo.myotherbrain.update.actions.CreateFirstClassItem;
import net.fortytwo.myotherbrain.update.actions.CreateGeoPoint;
import net.fortytwo.myotherbrain.update.actions.CreateLiteral;
import net.fortytwo.myotherbrain.update.actions.CreateWebResource;
import net.fortytwo.myotherbrain.update.actions.RemoveAlias;
import net.fortytwo.myotherbrain.update.actions.RemoveMarkerTag;
import net.fortytwo.myotherbrain.update.actions.SetDescription;
import net.fortytwo.myotherbrain.update.actions.SetEmphasis;
import net.fortytwo.myotherbrain.update.actions.SetIcon;
import net.fortytwo.myotherbrain.update.actions.SetName;
import net.fortytwo.myotherbrain.update.actions.SetSensitivity;
import org.json.JSONException;
import org.json.JSONObject;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * User: josh
 * Date: Jul 27, 2010
 * Time: 1:30:40 PM
 */
public class WriteActionParser {
    private static final DatatypeFactory DATATYPES;

    static {
        try {
            DATATYPES = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private final Map<String, WriteActionConstructor> constructors;

    public WriteActionParser() {
        constructors = new HashMap<String, WriteActionConstructor>();

        constructors.put("addAlias", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new AddAlias(
                        getURI(j, Param.subject),
                        getURI(j, Param.newAlias),
                        c);
            }
        });
        constructors.put("addMarkerTag", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new AddMarkerTag(
                        getURI(j, Param.subject),
                        getURI(j, Param.newMarkerTag),
                        c);
            }
        });
        constructors.put("breakAssociation", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new BreakAssociation(
                        getURI(j, Param.subject),
                        c);
            }
        });
        constructors.put("createAssociation", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateAssociation(
                        getURI(j, Param.subject),
                        optString(j, Param.name),
                        optString(j, Param.description),
                        optString(j, Param.richTextDescription),
                        optURI(j, Param.icon),
                        optURI(j, Param.sensitivity),
                        optFloat(j, Param.emphasis),
                        optDate(j, Param.creationTimeStamp),
                        optURI(j, Param.creationPlaceStamp),
                        getURI(j, Param.associationSubject),
                        getURI(j, Param.associationObject),
                        c);
            }
        });
        constructors.put("createFirstClassItem", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateFirstClassItem(
                        getURI(j, Param.subject),
                        optString(j, Param.name),
                        optString(j, Param.description),
                        optString(j, Param.richTextDescription),
                        optURI(j, Param.icon),
                        optURI(j, Param.sensitivity),
                        optFloat(j, Param.emphasis),
                        optDate(j, Param.creationTimeStamp),
                        optURI(j, Param.creationPlaceStamp),
                        c);
            }
        });
        constructors.put("createGeoPoint", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateGeoPoint(
                        getURI(j, Param.subject),
                        getFloat(j, Param.longitude),
                        getFloat(j, Param.latitude),
                        c);
            }
        });
        constructors.put("createLiteral", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateLiteral(
                        getURI(j, Param.subject),
                        optString(j, Param.name),
                        optString(j, Param.description),
                        optString(j, Param.richTextDescription),
                        optURI(j, Param.icon),
                        optURI(j, Param.sensitivity),
                        optFloat(j, Param.emphasis),
                        optDate(j, Param.creationTimeStamp),
                        optURI(j, Param.creationPlaceStamp),
                        getString(j, Param.lexicalForm),
                        optURI(j, Param.datatypeURI),
                        optString(j, Param.languageTag),
                        c);
            }
        });
        constructors.put("createWebResource", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateWebResource(
                        getURI(j, Param.subject),
                        optString(j, Param.representationMediaType),
                        optString(j, Param.representationSha1Sum),
                        c);
            }
        });
        constructors.put("removeAlias", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new RemoveAlias(
                        getURI(j, Param.subject),
                        getURI(j, Param.targetAlias),
                        c);
            }
        });
        constructors.put("removeMarkerTag", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new RemoveMarkerTag(
                        getURI(j, Param.subject),
                        getURI(j, Param.targetMarkerTag),
                        c);
            }
        });
        constructors.put("setDescription", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetDescription(
                        getURI(j, Param.subject),
                        getString(j, Param.description),
                        optString(j, Param.richTextDescription),
                        c);
            }
        });
        constructors.put("setEmphasis", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetEmphasis(
                        getURI(j, Param.subject),
                        getFloat(j, Param.emphasis),
                        c);
            }
        });
        constructors.put("setIcon", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetIcon(
                        getURI(j, Param.subject),
                        getURI(j, Param.icon),
                        c);
            }
        });
        constructors.put("setName", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetName(
                        getURI(j, Param.subject),
                        getString(j, Param.name),
                        c);
            }
        });
        constructors.put("setSensitivity", new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetSensitivity(
                        getURI(j, Param.subject),
                        getURI(j, Param.sensitivity),
                        c);
            }
        });
    }

    public WriteAction parse(final JSONObject j,
                             final WriteContext c) throws UpdateException {
        String name;
        JSONObject params;

        try {
            name = j.getString("action");
            params = j.getJSONObject("params");
        } catch (JSONException e) {
            throw new UpdateException(e);
        }

        WriteActionConstructor cons = constructors.get(name);
        if (null == cons) {
            throw new WriteActionParseException("unfamiliar action: " + name);
        }

        return cons.construct(params, c);
    }

    private interface WriteActionConstructor {
        WriteAction construct(JSONObject j, WriteContext c) throws UpdateException;
    }

    // JSON ////////////////////////////////////////////////////////////////////

    protected static String getString(final JSONObject j,
                                      final Enum key) throws WriteActionParseException {
        try {
            return j.getString(key.toString());
        } catch (JSONException e) {
            throw WriteActionParseException.missingField(key);
        }
    }

    protected static Float getFloat(final JSONObject j,
                                    final Enum key) throws UpdateException {
        try {
            return (float) j.getDouble(key.toString());
        } catch (JSONException e) {
            throw new UpdateException(e);
        }
    }

    protected static URI getURI(final JSONObject j,
                                final Enum key) throws UpdateException {
        String s = getString(j, key);

        try {
            return new URI(s);
        } catch (URISyntaxException e) {
            // FIXME: not really a JSON exception
            throw new UpdateException(s);
        }
    }

    protected static Date getDate(final JSONObject j,
                                  final Enum key) throws WriteActionParseException {
        String s = getString(j, key);

        XMLGregorianCalendar c = DATATYPES.newXMLGregorianCalendar(s);
        return c.toGregorianCalendar().getTime();
    }

    protected static String optString(final JSONObject j,
                                      final Enum key) {
        return j.optString(key.toString());
    }

    protected static Float optFloat(final JSONObject j,
                                    final Enum key) {
        return (float) j.optDouble(key.toString());
    }

    protected static URI optURI(final JSONObject j,
                                final Enum key) throws UpdateException {
        String s = j.optString(key.toString());

        try {
            return null == s
                    ? null
                    : new URI(s);
        } catch (URISyntaxException e) {
            throw new UpdateException(s);
        }
    }

    protected static Date optDate(final JSONObject j,
                                  final Enum key) {
        String s = j.optString(key.toString());

        if (null == s) {
            return null;
        } else {
            XMLGregorianCalendar c = DATATYPES.newXMLGregorianCalendar(s);
            return c.toGregorianCalendar().getTime();
        }
    }

    public static enum Param {
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
