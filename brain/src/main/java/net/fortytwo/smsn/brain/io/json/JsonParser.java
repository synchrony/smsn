package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.PageParser;
import net.fortytwo.smsn.brain.model.dto.ListNodeDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;

public class JsonParser extends PageParser {

    @Override
    public Page parse(final InputStream in) throws IOException {
        JSONObject json = new JSONObject(in);
        try {
            return parse(json);
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    private Page parse(final JSONObject json) throws JSONException {
        Page page = PageDTO.createTransitional();

        if (json.has(JsonFormat.Keys.ID)) {
            TreeViews.setId(page.getContent(), new AtomId(json.getString(JsonFormat.Keys.ID)));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.TITLE)) {
            TreeViews.setTitle(page.getContent(), json.getString(SemanticSynchrony.PropertyKeys.TITLE));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.ALIAS)) {
            page.setAlias(json.getString(SemanticSynchrony.PropertyKeys.ALIAS));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.SHORTCUT)) {
            page.setShortcut(json.getString(SemanticSynchrony.PropertyKeys.SHORTCUT));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.SOURCE)) {
            page.setSource(json.getString(SemanticSynchrony.PropertyKeys.SOURCE));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.WEIGHT)) {
            page.setWeight((float) json.getDouble(SemanticSynchrony.PropertyKeys.WEIGHT));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.PRIORITY)) {
            page.setPriority((float) json.getDouble(SemanticSynchrony.PropertyKeys.PRIORITY));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.CREATED)) {
            page.setCreated(json.getLong(SemanticSynchrony.PropertyKeys.CREATED));
        }
        if (json.has(JsonFormat.Keys.NUMBER_OF_CHILDREN)) {
            page.getContent().setNumberOfChildren(json.optInt(JsonFormat.Keys.NUMBER_OF_CHILDREN));
        }
        if (json.has(JsonFormat.Keys.NUMBER_OF_PARENTS)) {
            page.getContent().setNumberOfParents(json.optInt(JsonFormat.Keys.NUMBER_OF_PARENTS));
        }

        JSONArray a = json.optJSONArray(JsonFormat.Keys.CHILDREN);
        if (null != a) {
            TreeNode<Link>[] children = new TreeNode[a.length()];
            for (int i = 0; i < a.length(); i++) {
                JSONObject jc = a.getJSONObject(i);

                children[i] = parse(jc).getContent();
            }
            page.getContent().setChildren(ListNodeDTO.fromArray());
        }

        return page;
    }
}
