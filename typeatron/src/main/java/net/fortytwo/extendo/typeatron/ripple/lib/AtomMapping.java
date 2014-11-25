package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.NoteQueries;
import net.fortytwo.extendo.brain.wiki.NoteParser;
import net.fortytwo.extendo.typeatron.ripple.ExtendoBrainClient;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import org.apache.http.HttpException;
import org.json.JSONException;

import java.io.IOException;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class AtomMapping extends PrimitiveStackMapping {
    private static final Logger logger = Logger.getLogger(GetAtomValueMapping.class.getName());

    protected final ExtendoBrainClient client;
    protected final Filter filter;

    protected AtomMapping(final ExtendoBrainClient client,
                          final Filter filter) {
        this.client = client;
        this.filter = filter;
    }

    protected Note toNote(Object o, boolean sync) throws RippleException {
        if (o instanceof String) {
            if (NoteParser.ID.matcher((String) o).matches()) {
                Note n = new Note();
                n.setId((String) o);
                o = n;
            } else {
                return null;
            }
        }

        if (o instanceof Note) {
            Note n = (Note) o;
            if (null != n.getValue() && !sync) {
                return n;
            } else {
                if (null == n.getId()) {
                    logger.warning("note with null id");
                    return null;
                } else if (sync) {
                    try {
                        n = client.view(n, 1, filter, NoteQueries.FORWARD_ADJACENCY, false);
                    } catch (JSONException e) {
                        throw new RippleException(e);
                    } catch (IOException e) {
                        throw new RippleException(e);
                    } catch (HttpException e) {
                        throw new RippleException(e);
                    }
                }

                return n;
            }
        } else {
            return null;
        }
    }
}
