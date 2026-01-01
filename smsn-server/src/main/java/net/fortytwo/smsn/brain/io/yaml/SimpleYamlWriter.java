package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.repository.AtomRepositoryInterface;
import org.yaml.snakeyaml.Yaml;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class SimpleYamlWriter extends NoteWriter {

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(SimpleYamlFormat.getInstance());
    }

    @Override
    public void doWrite(Context context) throws IOException {
        AtomRepositoryInterface repository = context.getAtomRepository();
        writeGraph(repository, context.getDestStream());
    }

    private void writeGraph(final AtomRepositoryInterface repository,
                            final OutputStream out) throws IOException {

        List<Object> atomsObj = new ArrayList<>();
        for (AtomId atomId : repository.getAllAtomIds()) {
            Atom atom = repository.load(atomId);
            Map<String, Object> obj = new HashMap<>();

            // Add ID
            obj.put("id", atom.id.value);

            // Add other properties
            if (atom.title != null) obj.put(SemanticSynchrony.PropertyKeys.TITLE, atom.title);
            if (atom.created != null) obj.put(SemanticSynchrony.PropertyKeys.CREATED, atom.created.value);
            if (atom.weight != null) obj.put(SemanticSynchrony.PropertyKeys.WEIGHT, atom.weight.value);
            if (atom.source != null) obj.put(SemanticSynchrony.PropertyKeys.SOURCE, atom.source.value);

            // Add optional properties using reflection
            addOptionalProperty(obj, atom, "alias", SemanticSynchrony.PropertyKeys.ALIAS);
            addOptionalProperty(obj, atom, "shortcut", SemanticSynchrony.PropertyKeys.SHORTCUT);
            addOptionalProperty(obj, atom, "text", SemanticSynchrony.PropertyKeys.TEXT);
            addOptionalProperty(obj, atom, "priority", SemanticSynchrony.PropertyKeys.PRIORITY);

            // Add children
            if (!atom.children.isEmpty()) {
                List<String> childIds = new LinkedList<>();
                for (AtomId childId : atom.children) {
                    childIds.add(childId.value);
                }
                obj.put(SemanticSynchrony.EdgeLabels.CHILDREN, childIds);
            }

            atomsObj.add(obj);
        }

        Yaml yaml = new Yaml();
        String output = yaml.dump(atomsObj);
        out.write(output.getBytes());
    }

    private void addOptionalProperty(Map<String, Object> obj, Atom atom, String fieldName, String yamlKey) {
        try {
            Object optObj = atom.getClass().getField(fieldName).get(atom);
            Boolean isPresent = (Boolean) optObj.getClass().getMethod("isPresent").invoke(optObj);
            if (isPresent) {
                Object value = optObj.getClass().getMethod("get").invoke(optObj);
                obj.put(yamlKey, value);
            }
        } catch (Exception e) {
            // Ignore - field not present
        }
    }
}
