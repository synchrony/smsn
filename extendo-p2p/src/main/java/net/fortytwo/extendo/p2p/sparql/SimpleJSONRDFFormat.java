package net.fortytwo.extendo.p2p.sparql;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.query.Binding;
import org.openrdf.query.BindingSet;
import org.openrdf.query.impl.MapBindingSet;

import java.util.Collection;
import java.util.Iterator;

/**
 * A simple RDF format used for embedding RDF data in JSON-based messages
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SimpleJSONRDFFormat {

    private static final String
            FIELD_BNODE = "bnode",
            FIELD_DATATYPE = "datatype",
            FIELD_LANG = "lang",
            FIELD_LITERAL = "literal",
            FIELD_TYPE = "type",
            FIELD_URI = "uri",
            FIELD_VALUE = "value";

    private final ValueFactory valueFactory;

    public SimpleJSONRDFFormat(final ValueFactory valueFactory) {
        this.valueFactory = valueFactory;
    }

    public JSONArray statementsToJSON(final Statement... statements) throws JSONException {
        JSONArray all = new JSONArray();

        int count = 0;
        for (Statement s : statements) {
            all.put(count++, toJSON(s));
        }

        return all;
    }

    public JSONArray statementsToJSON(final Collection<Statement> statements) throws JSONException {
        JSONArray all = new JSONArray();

        int count = 0;
        for (Statement s : statements) {
            all.put(count++, toJSON(s));
        }

        return all;
    }

    private JSONArray toJSON(final Statement s) throws JSONException {
        JSONArray a = new JSONArray();

        a.put(0, toJSON(s.getSubject()));
        a.put(1, toJSON(s.getPredicate()));
        a.put(2, toJSON(s.getObject()));
        if (null != s.getContext()) {
            a.put(3, s.getContext());
        }

        return a;
    }

    private JSONObject toJSON(final Value v) throws JSONException {
        JSONObject j = new JSONObject();

        if (v instanceof Literal) {
            j.put(FIELD_VALUE, v.stringValue());

            j.put(FIELD_TYPE, FIELD_LITERAL);
            final Literal l = (Literal) v;

            if (null != l.getLanguage()) {
                j.put(FIELD_LANG, l.getLanguage());
            } else if (null != l.getDatatype()) {
                j.put(FIELD_DATATYPE, l.getDatatype().stringValue());
            }
        } else if (v instanceof BNode) {
            j.put(FIELD_VALUE, "_:" + ((BNode) v).getID());

            j.put(FIELD_TYPE, FIELD_BNODE);
        } else if (v instanceof org.openrdf.model.URI) {
            j.put(FIELD_VALUE, v.stringValue());

            j.put(FIELD_TYPE, FIELD_URI);
        } else {
            throw new IllegalStateException("value is of unexpected type: " + v);
        }

        return j;
    }

    public Value fromJSON(final JSONObject j) throws ParseError {
        Value object;

        try {
            String type = j.getString(FIELD_TYPE);
            String value = j.getString(FIELD_VALUE);

            if (FIELD_LITERAL.equals(type)) {
                String lang = j.optString(FIELD_LANG);
                String datatype = j.optString(FIELD_DATATYPE);

                if (lang != null) {
                    object = valueFactory.createLiteral(value, lang);
                } else if (datatype != null) {
                    object = valueFactory.createLiteral(value, valueFactory.createURI(datatype));
                } else {
                    object = valueFactory.createLiteral(value);
                }
            } else if (FIELD_BNODE.equals(type)) {
                object = valueFactory.createBNode(value.substring(2));
            } else if (FIELD_URI.equals(type)) {
                object = valueFactory.createURI(value);
            } else {
                throw new ParseError("unexpected type: " + type);
            }
        } catch (JSONException e) {
            throw new ParseError(e);
        }

        return object;
    }

    public BindingSet toBindingSet(final JSONObject bindings) throws ParseError {
        try {
            MapBindingSet bs = new MapBindingSet();
            Iterator keys = bindings.keys();
            while (keys.hasNext()) {
                Object key = keys.next();
                if (!(key instanceof String)) {
                    throw new ParseError("String expected as key");
                }
                JSONObject value = bindings.getJSONObject((String) key);

                Value v = fromJSON(value);
                bs.addBinding((String) key, v);
            }

            return bs;
        } catch (JSONException e) {
            throw new ParseError(e);
        }
    }

    public JSONObject toJSON(final BindingSet bs) throws JSONException {
        JSONObject j = new JSONObject();

        for (Binding b : bs) {
            j.put(b.getName(), toJSON(b.getValue()));
        }

        return j;
    }

    public Statement toStatement(final JSONArray a) throws ParseError {
        int l = a.length();
        if (l < 3 || l > 4) {
            throw new ParseError("triple/quad must have three or four elements");
        }

        try {
            Value subject = fromJSON(a.getJSONObject(0));
            Value predicate = fromJSON(a.getJSONObject(1));
            Value object = fromJSON(a.getJSONObject(2));
            Value context = 4 == l ? fromJSON(a.getJSONObject(3)) : null;

            if (!(subject instanceof Resource)) {
                throw new ParseError("subject of statement must be an RDF resource");
            }

            if (!(predicate instanceof URI)) {
                throw new ParseError("predicate of statement must be a URI");
            }

            if (null != context && !(context instanceof Resource)) {
                throw new ParseError("graph context of statement must be an RDF resource");
            }

            return valueFactory.createStatement((Resource) subject, (URI) predicate, object, (Resource) context);
        } catch (JSONException e) {
            throw new ParseError(e);
        }
    }

    /**
    * @author Joshua Shinavier (http://fortytwo.net)
    */
    public static class ParseError extends Exception {
        public ParseError(final String message) {
            super(message);
        }

        public ParseError(final Throwable cause) {
            super(cause);
        }
    }
}
