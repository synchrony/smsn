package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.HydraEntityStore;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

/**
 * A read-only service for retrieving the Hydra entity record of a single
 * atom from the exploded kg-* files. An atom with no entity record of its
 * own yields an empty view by design, rather than falling back to a host
 * entity.
 *
 * The response carries the record in two forms: the record itself as Hydra
 * JSON (with a separate atomId-to-title map, so the record stays pure and
 * decodable by generated coders), and pre-rendered YAML views — the entity
 * instance alone, and the envelope metadata — for human-facing buffers.
 */
public class GetHydraView extends RootedViewAction {

    @Override
    protected void performTransaction(final ActionContext context)
            throws RequestProcessingException, BadRequestException {
        super.performTransaction(context);

        AtomId rootId = getRoot().id;
        if (null == HydraEntityStore.getDataDir()) {
            throw new RequestProcessingException("Hydra data directory not found ("
                    + HydraEntityStore.describeDataDirConfig() + ")");
        }
        context.getMap().put("hydraVintage", HydraEntityStore.getVintage());

        JSONObject record = HydraEntityStore.find(rootId.value);
        if (null == record) {
            context.getMap().put("hydraFound", false);
        } else {
            JSONObject titles = HydraYaml.collectTitles(record, id ->
                    context.getRepository().findById(new AtomId(id)).map(atom -> atom.title).orElse(null));

            // the instance field is a TypedInstance union value; the buffer
            // shows exactly the underlying record, with the variant tag
            // reported separately as the entity's type name
            Object instance = record.opt("instance");
            String typeName = null;
            if (instance instanceof JSONObject && 1 == ((JSONObject) instance).length()) {
                typeName = ((JSONObject) instance).keys().next();
                instance = ((JSONObject) instance).opt(typeName);
            }
            JSONObject envelope = new JSONObject(record.toString());
            envelope.remove("instance");

            context.getMap().put("hydraFound", true);
            if (null != typeName) {
                context.getMap().put("hydraTypeName", typeName);
            }
            context.getMap().put("hydraSource", record.optString("source"));
            context.getMap().put("hydraEntity", record);
            context.getMap().put("hydraTitles", titles);
            context.getMap().put("hydraEntityYaml",
                    HydraYaml.indentAndComment(HydraYaml.toYaml(instance, titles)));
            context.getMap().put("hydraMetaYaml",
                    HydraYaml.indentAndComment(HydraYaml.toYaml(envelope, titles)));
        }

        addToHistory(rootId);
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
