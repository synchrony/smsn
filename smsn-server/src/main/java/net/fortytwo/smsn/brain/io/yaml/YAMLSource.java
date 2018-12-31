package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.model.dto.ListNodeDTO;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
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

    public void add(final Note a) {
        addAtom(a);
    }

    private Map<String, Object> createMap() {
        // Use an order-preserving map so the column output is deterministic.
        return new LinkedHashMap<>();
    }

    private void addAtom(final Note a) {
        Map<String, Object> map = createMap();
        toMap(map, a, YAMLFormat.Constants.AtomFields.ALIAS, Note::getAlias);

        if (null != a.getChildren()) {
            List<Note> children = ListNode.toJavaList(a.getChildren());
            List<String> ids = children.stream().map(note -> Note.getId(note)).collect(Collectors.toList());
            map.put(YAMLFormat.Constants.AtomFields.CHILDREN, ids);
        }

        toMap(map, a, YAMLFormat.Constants.AtomFields.CREATED, Note::getCreated);
        toMap(map, a, YAMLFormat.Constants.AtomFields.ID, Note::getId);
        toMap(map, a, YAMLFormat.Constants.AtomFields.PRIORITY, Note::getPriority);
        toMap(map, a, YAMLFormat.Constants.AtomFields.SHORTCUT, Note::getShortcut);
        toMap(map, a, YAMLFormat.Constants.AtomFields.TEXT, Note::getText);
        toMap(map, a, YAMLFormat.Constants.AtomFields.TITLE, Note::getTitle);
        toMap(map, a, YAMLFormat.Constants.AtomFields.WEIGHT, Note::getWeight);

        atoms.add(map);
    }

    private void toMap(final Map<String, Object> map,
                       final Note a,
                       final String key,
                       final Function<Note, Object> getter) {
        Object value = getter.apply(a);
        if (null != value) {
            map.put(key, value);
        }
    }

    private void fromMap(final Map<String, Object> map,
                         final Note a,
                         final String key,
                         final BiConsumer<Note, Object> setter) {
        Object o = map.get(key);
        if (null != o) {
            setter.accept(a, o);
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

    public Collection<Note> readFrom(final File dir) throws IOException {
        Map<String, Note> notes = readAtoms(dir);
        return notes.values();
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

    private Map<String, Note> readAtoms(final File dir) throws IOException {
        File file = atomFile(dir);
        Map<String, Note> notes = new HashMap<>();

        for (Map<String, Object> map : loadYaml(file)) {
            Note note = new NoteDTO();
            Note.setSource(note, sourceName);

            fromMap(map, note, YAMLFormat.Constants.AtomFields.ALIAS, (n, o) -> Note.setAlias(n, toString(o)));
            fromMap(map, note, YAMLFormat.Constants.AtomFields.CREATED, (n, o) -> Note.setCreated(n, (Long) o));
            fromMap(map, note, YAMLFormat.Constants.AtomFields.ID, (n, o) -> Note.setId(n, toString(o)));
            fromMap(map, note, YAMLFormat.Constants.AtomFields.PRIORITY, (n, o) -> Note.setPriority(n, toFloat(o)));
            fromMap(map, note, YAMLFormat.Constants.AtomFields.SHORTCUT, (n, o) -> Note.setShortcut(n, toString(o)));
            fromMap(map, note, YAMLFormat.Constants.AtomFields.TEXT, (n, o) -> Note.setText(n, toString(o)));
            fromMap(map, note, YAMLFormat.Constants.AtomFields.TITLE, (n, o) -> Note.setTitle(n, toString(o)));
            fromMap(map, note, YAMLFormat.Constants.AtomFields.WEIGHT, (n, o) -> Note.setWeight(n, toFloat(o)));

            List<String> children = (List<String>) map.get(YAMLFormat.Constants.AtomFields.CHILDREN);
            if (null != children) {
                note.setChildren(fromIds(children));
            }

            notes.put(Note.getId(note), note);
        }

        return notes;
    }

    private Float toFloat(final Object o) {
        return ((Double) o).floatValue();
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

    private ListNode<Note> fromIds(final List<String> ids) {
        Note[] notes = new Note[ids.size()];
        int i = 0;
        for (String id : ids) {
            Note note = new NoteDTO();
            Note.setId(note, id);
            notes[i++] = note;
        }
        return ListNodeDTO.fromArray(notes);
    }

    private List<Map<String, Object>> loadYaml(final File file) throws IOException {
        try (InputStream in = new FileInputStream(file)) {
            return (List<Map<String, Object>>) new Yaml().load(in);
        }
    }
}
