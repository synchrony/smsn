package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.model.entities.Note;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

class YAMLGraph {
    private Map<String, YAMLSource> sourceGraphs = new HashMap<>();

    public void addAtom(final Note a) {
        String source = Note.getSource(a);
        // A null source means that the atom is just a reference, and is not described in any of the owned graphs.
        if (null != source) {
            getSource(source).add(a);
        }
    }

    public void writeTo(final Map<String, File> sourceDirs) throws IOException {
        for (Map.Entry<String, YAMLSource> e : sourceGraphs.entrySet()) {
            File dir = sourceDirs.get(e.getKey());
            if (null == dir) {
                throw new IllegalStateException("no directory for source: " + e.getKey());
            }
            e.getValue().writeTo(dir);
        }
    }

    public Collection<Note> readFrom(final Map<String, File> sourceDirs) throws IOException {
        Collection<Note> notes = new LinkedList<>();
        for (Map.Entry<String, File> e : sourceDirs.entrySet()) {
            YAMLSource source = getSource(e.getKey());
            notes.addAll(source.readFrom(e.getValue()));
        }
        return notes;
    }

    private YAMLSource getSource(final String name) {
        YAMLSource g = sourceGraphs.get(name);
        if (null == g) {
            g = new YAMLSource(name);
            sourceGraphs.put(name, g);
        }
        return g;
    }
}
