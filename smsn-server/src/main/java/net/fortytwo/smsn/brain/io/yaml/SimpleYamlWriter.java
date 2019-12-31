package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.pg.PGTopicGraph;
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
        if (!(context.getTopicGraph() instanceof PGTopicGraph)) throw new UnsupportedOperationException();
        PGTopicGraph graph = (PGTopicGraph) context.getTopicGraph();

        writeGraph(graph, context.getDestStream());
    }

    private void writeGraph(final PGTopicGraph graph,
                            final OutputStream out) throws IOException {

        List<Object> notesObj = new ArrayList<Object>();
        for (Note note : graph.getAllNotes()) {
            Map<String, Object> obj = new HashMap<>();

            for (String key : SemanticSynchrony.PropertyKeys.allPropertyKeys) {
                Object value = note.optProperty(key);
                if (null != value) {
                    key = key.equals(SemanticSynchrony.PropertyKeys.ID) ? "id" : key;
                    obj.put(key, value);
                }
            }

            List<Note> children = ListNode.toJavaList(note.getChildren());
            if (!children.isEmpty()) {
                List<String> childIds = new LinkedList<>();
                for (Note child : children) {
                    childIds.add(Note.getId(child));
                }
                obj.put(SemanticSynchrony.EdgeLabels.CHILDREN, childIds);
            }

            notesObj.add(obj);
        }

        Yaml yaml = new Yaml();
        String output = yaml.dump(notesObj);
        out.write(output.getBytes());
    }
}
