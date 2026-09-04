package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.HydraEntityStore;
import org.json.JSONArray;
import org.json.JSONObject;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Renders Hydra entity records as canonical YAML for human-facing views:
 * block style, two-space indent, sorted keys, no line splitting. Resolved
 * atom titles are appended to atomId lines as parse-inert YAML comments,
 * so the emitted text remains valid YAML that round-trips.
 *
 * YAML is deliberately produced outside of Hydra; the typed interchange
 * format between client and server remains Hydra JSON.
 */
final class HydraYaml {

    private static final Pattern ATOM_ID_LINE =
            Pattern.compile("^(\\s*(?:- )?)atomId: ([A-Za-z0-9@&_-]+)\\s*$");

    private HydraYaml() {
    }

    /**
     * Render a JSON value (typically a JSONObject) as annotated YAML.
     * Titles maps atom ids to resolved titles for comment annotation.
     */
    static String toYaml(final Object json, final JSONObject titles) {
        Object plain = toPlain(json);
        if (null == plain) {
            return "";
        }
        Yaml yaml = new Yaml(dumperOptions());
        return annotate(yaml.dump(plain), titles);
    }

    private static DumperOptions dumperOptions() {
        DumperOptions options = new DumperOptions();
        options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
        options.setIndent(2);
        options.setAllowUnicode(true);
        options.setSplitLines(false);
        options.setWidth(1_000_000);
        return options;
    }

    /**
     * Convert org.json values to plain Java maps and lists for SnakeYAML,
     * sorting keys for a canonical, byte-stable rendering.
     */
    private static Object toPlain(final Object json) {
        if (JSONObject.NULL.equals(json)) {
            return null;
        } else if (json instanceof JSONObject) {
            JSONObject obj = (JSONObject) json;
            Map<String, Object> map = new TreeMap<>();
            for (Iterator<String> it = obj.keys(); it.hasNext(); ) {
                String key = it.next();
                map.put(key, toPlain(obj.opt(key)));
            }
            return map;
        } else if (json instanceof JSONArray) {
            JSONArray array = (JSONArray) json;
            List<Object> list = new ArrayList<>();
            for (int i = 0; i < array.length(); i++) {
                list.add(toPlain(array.opt(i)));
            }
            return list;
        } else {
            return json;
        }
    }

    private static String annotate(final String yaml, final JSONObject titles) {
        if (null == titles || titles.length() == 0) {
            return yaml;
        }
        StringBuilder sb = new StringBuilder();
        for (String line : yaml.split("\n", -1)) {
            Matcher matcher = ATOM_ID_LINE.matcher(line);
            if (matcher.matches()) {
                String title = titles.optString(matcher.group(2), null);
                if (null != title) {
                    line = line + "  # → " + title.replace('\n', ' ');
                }
            }
            sb.append(line).append('\n');
        }
        // split() with a trailing newline yields one empty trailing element
        if (sb.length() > 0 && yaml.endsWith("\n")) {
            sb.setLength(sb.length() - 1);
        }
        return sb.toString();
    }

    /**
     * Collect resolved titles for every AtomRef in the given record, looking
     * each atom id up in the repository via the provided resolver.
     */
    static JSONObject collectTitles(final Object node, final java.util.function.Function<String, String> resolver) {
        JSONObject titles = new JSONObject();
        collectTitles(node, resolver, titles);
        return titles;
    }

    private static void collectTitles(final Object node,
                                      final java.util.function.Function<String, String> resolver,
                                      final JSONObject titles) {
        if (node instanceof JSONObject) {
            JSONObject obj = (JSONObject) node;
            Object id = obj.opt("atomId");
            if (id instanceof String && HydraEntityStore.SAFE_ATOM_ID.matcher((String) id).matches()
                    && !titles.has((String) id)) {
                String title = resolver.apply((String) id);
                if (null != title) {
                    titles.put((String) id, title);
                }
            }
            for (Iterator<String> it = obj.keys(); it.hasNext(); ) {
                collectTitles(obj.opt(it.next()), resolver, titles);
            }
        } else if (node instanceof JSONArray) {
            JSONArray array = (JSONArray) node;
            for (int i = 0; i < array.length(); i++) {
                collectTitles(array.opt(i), resolver, titles);
            }
        }
    }
}
