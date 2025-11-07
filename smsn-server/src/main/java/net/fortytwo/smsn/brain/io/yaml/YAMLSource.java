package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Timestamp;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.constructor.Constructor;
import org.yaml.snakeyaml.representer.Representer;
import org.yaml.snakeyaml.resolver.Resolver;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import static net.fortytwo.smsn.brain.io.yaml.YAMLFormat.YAML_EXTENSION;

class YAMLSource {
    private final String sourceName;
    private final List<Map<String, Object>> atoms = new LinkedList<>();

    YAMLSource(String sourceName) {
        this.sourceName = sourceName;
    }

    public void add(final Atom atom) {
        addAtom(atom);
    }

    private Map<String, Object> createMap() {
        // Use an order-preserving map so the column output is deterministic.
        return new LinkedHashMap<>();
    }

    private void addAtom(final Atom atom) {
        Map<String, Object> map = createMap();

        // Add ID (required)
        map.put(YAMLFormat.Constants.AtomFields.ID, atom.id.value);

        // Add title (required)
        if (atom.title != null) {
            map.put(YAMLFormat.Constants.AtomFields.TITLE, atom.title);
        }

        // Add optional Opt<String> fields using reflection
        addOptionalStringField(map, atom, "alias", YAMLFormat.Constants.AtomFields.ALIAS);
        addOptionalStringField(map, atom, "shortcut", YAMLFormat.Constants.AtomFields.SHORTCUT);
        addOptionalStringField(map, atom, "text", YAMLFormat.Constants.AtomFields.TEXT);

        // Add optional Opt<Normed> priority field using reflection
        addOptionalNormedField(map, atom, "priority", YAMLFormat.Constants.AtomFields.PRIORITY);

        // Add children
        if (!atom.children.isEmpty()) {
            List<String> ids = atom.children.stream().map(id -> id.value).collect(Collectors.toList());
            map.put(YAMLFormat.Constants.AtomFields.CHILDREN, ids);
        }

        // Add created
        if (atom.created != null) {
            map.put(YAMLFormat.Constants.AtomFields.CREATED, atom.created.value);
        }

        // Add weight
        if (atom.weight != null) {
            map.put(YAMLFormat.Constants.AtomFields.WEIGHT, atom.weight.value);
        }

        atoms.add(map);
    }

    private void addOptionalStringField(Map<String, Object> map, Atom atom, String fieldName, String yamlKey) {
        try {
            Object optObj = atom.getClass().getField(fieldName).get(atom);
            Boolean isPresent = (Boolean) optObj.getClass().getMethod("isPresent").invoke(optObj);
            if (isPresent) {
                Object value = optObj.getClass().getMethod("get").invoke(optObj);
                map.put(yamlKey, value);
            }
        } catch (Exception e) {
            // Ignore - field not present
        }
    }

    private void addOptionalNormedField(Map<String, Object> map, Atom atom, String fieldName, String yamlKey) {
        try {
            Object optObj = atom.getClass().getField(fieldName).get(atom);
            Boolean isPresent = (Boolean) optObj.getClass().getMethod("isPresent").invoke(optObj);
            if (isPresent) {
                Object normedObj = optObj.getClass().getMethod("get").invoke(optObj);
                Object value = normedObj.getClass().getField("value").get(normedObj);
                map.put(yamlKey, value);
            }
        } catch (Exception e) {
            // Ignore - field not present
        }
    }

    // Sort the collections so the row output is deterministic.
    private void sort() {
        Collections.sort(atoms, (o1, o2) ->
                ((Comparable) o1.get(YAMLFormat.Constants.AtomFields.ID))
                .compareTo(o2.get(YAMLFormat.Constants.AtomFields.ID)));
    }

    public void writeTo(final File dir) throws IOException {
        sort();

        writeFile(atomFile(dir), atoms);
    }

    public Collection<Atom> readFrom(final File dir) throws IOException {
        Map<AtomId, Atom> atomsMap = readAtoms(dir);
        return atomsMap.values();
    }

    private File atomFile(final File dir) {
        return new File(dir, YAMLFormat.Constants.ATOM + "." + YAML_EXTENSION);
    }

    private void writeFile(final File file, final List<Map<String, Object>> data) throws IOException {
        try (OutputStream out = new FileOutputStream(file)) {
            DumperOptions options = new DumperOptions();
            options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
            Yaml yaml = new Yaml(options);

            yaml.dump(data, new OutputStreamWriter(out));
        }
    }

    private Map<AtomId, Atom> readAtoms(final File dir) throws IOException {
        File file = atomFile(dir);
        Map<AtomId, Atom> atomsMap = new HashMap<>();

        for (Map<String, Object> map : loadYaml(file)) {
            // Extract required fields
            AtomId id = new AtomId(toString(map.get(YAMLFormat.Constants.AtomFields.ID)));
            Object titleObj = map.get(YAMLFormat.Constants.AtomFields.TITLE);
            String title = titleObj != null ? toString(titleObj) : "";

            // Extract children
            List<AtomId> children = new ArrayList<>();
            List<String> childIds = (List<String>) map.get(YAMLFormat.Constants.AtomFields.CHILDREN);
            if (childIds != null) {
                for (String childId : childIds) {
                    children.add(new AtomId(childId));
                }
            }

            // Extract timestamp (required field)
            Timestamp created = extractTimestamp(map);

            // Extract weight (required field)
            Normed weight = extractWeight(map);

            // Create Atom using reflection to avoid direct Opt import
            Atom atom = createAtom(
                id,
                created,
                weight,
                extractOptNormed(map, YAMLFormat.Constants.AtomFields.PRIORITY),
                new SourceName(sourceName),
                title,
                extractOptString(map, YAMLFormat.Constants.AtomFields.ALIAS),
                extractOptString(map, YAMLFormat.Constants.AtomFields.TEXT),
                extractOptString(map, YAMLFormat.Constants.AtomFields.SHORTCUT),
                children
            );

            atomsMap.put(id, atom);
        }

        return atomsMap;
    }

    @SuppressWarnings("unchecked")
    private Atom createAtom(AtomId id, Timestamp created, Normed weight, Object priority,
                            SourceName source, String title, Object alias, Object text,
                            Object shortcut, List<AtomId> children) {
        try {
            // Use reflection to call Atom constructor
            Class<?> atomClass = Atom.class;
            java.lang.reflect.Constructor<?> constructor = atomClass.getConstructor(
                AtomId.class,
                Timestamp.class,
                Normed.class,
                Class.forName("hydra.util.Opt"),  // priority
                SourceName.class,
                String.class,
                Class.forName("hydra.util.Opt"),  // alias
                Class.forName("hydra.util.Opt"),  // text
                Class.forName("hydra.util.Opt"),  // shortcut
                java.util.List.class
            );
            return (Atom) constructor.newInstance(id, created, weight, priority, source, title, alias, text, shortcut, children);
        } catch (Exception e) {
            throw new RuntimeException("Failed to create Atom", e);
        }
    }

    private Timestamp extractTimestamp(Map<String, Object> map) {
        Object obj = map.get(YAMLFormat.Constants.AtomFields.CREATED);
        // Default to current time if not present
        // Timestamp is in seconds (Integer), not milliseconds
        return obj != null ? new Timestamp(toInt(obj)) : new Timestamp((int) (System.currentTimeMillis() / 1000));
    }

    private Normed extractWeight(Map<String, Object> map) {
        Object obj = map.get(YAMLFormat.Constants.AtomFields.WEIGHT);
        // Default weight to 0.5 if not present
        return obj != null ? new Normed(toFloat(obj)) : new Normed(0.5f);
    }

    @SuppressWarnings("unchecked")
    private Object extractOptString(Map<String, Object> map, String key) {
        Object obj = map.get(key);
        try {
            Class<?> optClass = Class.forName("hydra.util.Opt");
            if (obj != null) {
                return optClass.getMethod("of", Object.class).invoke(null, toString(obj));
            } else {
                return optClass.getMethod("empty").invoke(null);
            }
        } catch (Exception e) {
            throw new RuntimeException("Failed to create Opt for " + key, e);
        }
    }

    @SuppressWarnings("unchecked")
    private Object extractOptNormed(Map<String, Object> map, String key) {
        Object obj = map.get(key);
        try {
            Class<?> optClass = Class.forName("hydra.util.Opt");
            if (obj != null) {
                return optClass.getMethod("of", Object.class).invoke(null, new Normed(toFloat(obj)));
            } else {
                return optClass.getMethod("empty").invoke(null);
            }
        } catch (Exception e) {
            throw new RuntimeException("Failed to create Opt for " + key, e);
        }
    }

    private Integer toInt(final Object o) {
        return o instanceof Number
                ? ((Number) o).intValue()
                : Integer.valueOf(o.toString());
    }

    private Float toFloat(final Object o) {
        return o instanceof Number
                ? ((Number) o).floatValue()
                : Float.valueOf(o.toString());
    }

    private Long toLong(final Object o) {
        return o instanceof Number
                ? ((Number) o).longValue()
                : Long.valueOf(o.toString());
    }

    private String toString(final Object o) {
        if (o instanceof String) {
            return (String) o;
        } else if (o instanceof byte[]) {
            return new String((byte[]) o);
        } else {
            throw new IllegalStateException("object of class " + o.getClass()
                    + " cannot be converted to a String: " + o);
        }
    }

    private static Resolver createResolver() {
        return new CustomResolver();
    }

    private List<Map<String, Object>> loadYaml(final File file) throws IOException {
        try (InputStream in = new FileInputStream(file)) {
            //Yaml yaml = new Yaml();
            LoaderOptions options = new LoaderOptions();
            Representer representer = new Representer(new DumperOptions());
            Yaml yaml = new Yaml(new Constructor(options), representer, new DumperOptions(), createResolver());
            return yaml.load(in);
        }
    }
}
