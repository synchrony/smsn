package net.fortytwo.myotherbrain.update;

import net.fortytwo.myotherbrain.update.actions.AddAlias;
import net.fortytwo.myotherbrain.update.actions.AddMarkerTag;
import net.fortytwo.myotherbrain.update.actions.BreakAssociation;
import net.fortytwo.myotherbrain.update.actions.CreateAssociation;
import net.fortytwo.myotherbrain.update.actions.CreateAtom;
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

    private final Map<JSONRemoting.WriteActionType, WriteActionConstructor> constructors;

    public WriteActionParser() {
        constructors = new HashMap<JSONRemoting.WriteActionType, WriteActionConstructor>();

        constructors.put(JSONRemoting.WriteActionType.addAlias, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new AddAlias(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getURI(j, JSONRemoting.WriteActionParam.newAlias),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.addMarkerTag, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new AddMarkerTag(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getURI(j, JSONRemoting.WriteActionParam.newMarkerTag),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.breakAssociation, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new BreakAssociation(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.createAssociation, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateAssociation(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        optString(j, JSONRemoting.WriteActionParam.name),
                        optString(j, JSONRemoting.WriteActionParam.description),
                        optString(j, JSONRemoting.WriteActionParam.richTextDescription),
                        optURI(j, JSONRemoting.WriteActionParam.icon),
                        optURI(j, JSONRemoting.WriteActionParam.sensitivity),
                        optFloat(j, JSONRemoting.WriteActionParam.emphasis),
                        optDate(j, JSONRemoting.WriteActionParam.creationTimeStamp),
                        optURI(j, JSONRemoting.WriteActionParam.creationPlaceStamp),
                        getURI(j, JSONRemoting.WriteActionParam.associationSubject),
                        getURI(j, JSONRemoting.WriteActionParam.associationObject),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.createAtom, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateAtom(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        optString(j, JSONRemoting.WriteActionParam.name),
                        optString(j, JSONRemoting.WriteActionParam.description),
                        optString(j, JSONRemoting.WriteActionParam.richTextDescription),
                        optURI(j, JSONRemoting.WriteActionParam.icon),
                        optURI(j, JSONRemoting.WriteActionParam.sensitivity),
                        optFloat(j, JSONRemoting.WriteActionParam.emphasis),
                        optDate(j, JSONRemoting.WriteActionParam.creationTimeStamp),
                        optURI(j, JSONRemoting.WriteActionParam.creationPlaceStamp),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.createGeoPoint, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateGeoPoint(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getFloat(j, JSONRemoting.WriteActionParam.longitude),
                        getFloat(j, JSONRemoting.WriteActionParam.latitude),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.createLiteral, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateLiteral(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        optString(j, JSONRemoting.WriteActionParam.name),
                        optString(j, JSONRemoting.WriteActionParam.description),
                        optString(j, JSONRemoting.WriteActionParam.richTextDescription),
                        optURI(j, JSONRemoting.WriteActionParam.icon),
                        optURI(j, JSONRemoting.WriteActionParam.sensitivity),
                        optFloat(j, JSONRemoting.WriteActionParam.emphasis),
                        optDate(j, JSONRemoting.WriteActionParam.creationTimeStamp),
                        optURI(j, JSONRemoting.WriteActionParam.creationPlaceStamp),
                        getString(j, JSONRemoting.WriteActionParam.lexicalForm),
                        optURI(j, JSONRemoting.WriteActionParam.datatypeURI),
                        optString(j, JSONRemoting.WriteActionParam.languageTag),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.createWebResource, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new CreateWebResource(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        optString(j, JSONRemoting.WriteActionParam.representationMediaType),
                        optString(j, JSONRemoting.WriteActionParam.representationSha1Sum),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.removeAlias, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new RemoveAlias(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getURI(j, JSONRemoting.WriteActionParam.targetAlias),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.removeMarkerTag, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new RemoveMarkerTag(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getURI(j, JSONRemoting.WriteActionParam.targetMarkerTag),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.setDescription, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetDescription(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getString(j, JSONRemoting.WriteActionParam.description),
                        optString(j, JSONRemoting.WriteActionParam.richTextDescription),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.setEmphasis, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetEmphasis(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getFloat(j, JSONRemoting.WriteActionParam.emphasis),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.setIcon, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetIcon(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getURI(j, JSONRemoting.WriteActionParam.icon),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.setName, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetName(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getString(j, JSONRemoting.WriteActionParam.name),
                        c);
            }
        });
        constructors.put(JSONRemoting.WriteActionType.setSensitivity, new WriteActionConstructor() {
            public WriteAction construct(JSONObject j, WriteContext c) throws UpdateException {
                return new SetSensitivity(
                        getURI(j, JSONRemoting.WriteActionParam.subject),
                        getURI(j, JSONRemoting.WriteActionParam.sensitivity),
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

        JSONRemoting.WriteActionType type;

        try {
            type = JSONRemoting.WriteActionType.valueOf(name);
        } catch (IllegalArgumentException e) {
            throw new WriteActionParseException("unfamiliar action: " + name);
        }
        WriteActionConstructor cons = constructors.get(type);
        if (null == cons) {
            throw new IllegalStateException("no constructor for type " + type);
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
}
