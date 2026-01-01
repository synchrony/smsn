package net.fortytwo.smsn.server.actions;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.vcs.AtomVCSWriter;
import net.fortytwo.smsn.brain.model.pg.PGTopicGraph;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.config.Configuration;
import net.fortytwo.smsn.config.DataSource;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Property;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.Optional;
import java.util.function.Function;

/**
 * A service to adjust a graph to data model changes
 */
public class ActionDuJour extends Action {

    @Override
    protected void performTransaction(ActionContext context) throws BadRequestException, RequestProcessingException {
        // add an "action du jour" as needed

        try {
            //pageToText(context);

            //migrateIds(context);

            //findAnomalousNotes(context);

            //sharabilityToSource(context);
        } catch (Exception e) {
            throw new RequestProcessingException(e);
        }
    }

    private void addGraph(final File configFile) throws IOException {
        Configuration config = SemanticSynchrony.getConfiguration();
        try {
            try (InputStream input = new FileInputStream(configFile)) {
                SemanticSynchrony.readConfigurationYaml(input);
            }
            Configuration other = SemanticSynchrony.getConfiguration();
        } finally {
            SemanticSynchrony.setConfiguration(config);
        }
    }

    private void pageToText(final ActionContext context) {
        PGTopicGraph topicGraph = (PGTopicGraph) context.getBrain().getTopicGraph();
        Graph graph = topicGraph.getPropertyGraph();
        for (Iterator<Vertex> it = graph.vertices(); it.hasNext(); ) {
            Vertex v = it.next();
            if (isNoteVertex(v)) {
                VertexProperty<String> prop = v.property("page");
                if (prop.isPresent()) {
                    String text = prop.value();
                    prop.remove();
                    v.property(SemanticSynchrony.PropertyKeys.TEXT, text);
                }
            }
        }
    }

    private void sharabilityToSource(final ActionContext context) {
        PGTopicGraph topicGraph = (PGTopicGraph) context.getBrain().getTopicGraph();
        Graph graph = topicGraph.getPropertyGraph();
        for (Iterator<Vertex> it = graph.vertices(); it.hasNext(); ) {
            Vertex v = it.next();
            if (isNoteVertex(v)) {
                Property<String> source1 = v.property("source");
                if (!source1.isPresent()) {
                    String id = v.property(SemanticSynchrony.PropertyKeys.ID).value().toString();
                    VertexProperty<String> titleProp = v.property(SemanticSynchrony.PropertyKeys.TITLE);
                    String title = titleProp.isPresent() ? titleProp.value() : "(no title)";
                    System.out.println("note " + id + " has no source. Title: " + title);

                    Property<Float> sharability = v.property("sharability");
                    if (sharability.isPresent()) {
                        System.out.println("\tsharability: " + sharability.value());
                    } else {
                        System.out.println("\tno sharability");
                    }

                    String source = sourceForSharability(v);
                    if (null != source) {
                        v.property(SemanticSynchrony.PropertyKeys.SOURCE, source);
                    }
                }
            }
        }
    }

    private String sourceForSharability(final Vertex vertex) {
        Property<Float> sharability = vertex.property("sharability");
        if (sharability.isPresent()) {
            switch ((int) (sharability.value() * 4)) {
                case 0:
                    throw new IllegalStateException();
                case 1:
                    return "private";
                case 2:
                    return "personal";
                case 3:
                    return "public";
                case 4:
                    return "universal";
                default:
                    throw new IllegalStateException();
            }
        }

        return null;
    }

    private void assignSources(ActionContext context) {
        net.fortytwo.smsn.brain.repository.AtomRepositoryInterface repository = context.getRepository();
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            File dir = new File(source.getLocation());
            Preconditions.checkArgument(dir.exists() && dir.isDirectory());
            for (File file : dir.listFiles()) {
                if (net.fortytwo.smsn.brain.io.vcs.VCSFormat.FORMAT.isMatchingFile(file)) {
                    AtomId id = new AtomId(file.getName());
                    Optional<Atom> opt = repository.findById(id);
                    Preconditions.checkArgument(opt.isPresent());
                    repository.updateProperty(id, SemanticSynchrony.PropertyKeys.SOURCE, source.getName());
                }
            }
            //SmSnGitRepository repo = new SmSnGitRepository(context.getBrain(), source);

        }
    }

    private void migrateIds(final ActionContext context) {
        PGTopicGraph topicGraph = (PGTopicGraph) context.getBrain().getTopicGraph();
        Graph graph = topicGraph.getPropertyGraph();
        for (Iterator<Vertex> it = graph.vertices(); it.hasNext(); ) {
            Vertex v = it.next();
            if (isNoteVertex(v)) {
                VertexProperty<String> idProp = v.property(SemanticSynchrony.PropertyKeys.ID);
                if (idProp.isPresent()) {
                    AtomId oldId = new AtomId(idProp.value());
                    AtomId newId = SemanticSynchrony.migrateId(oldId);
                    v.property(SemanticSynchrony.PropertyKeys.ID, newId.value);
                }
            }
        }
    }

    private void findAnomalousNotes(final ActionContext context) {
        PGTopicGraph topicGraph = (PGTopicGraph) context.getBrain().getTopicGraph();
        Graph graph = topicGraph.getPropertyGraph();
        for (Iterator<Vertex> it = graph.vertices(); it.hasNext(); ) {
            Vertex v = it.next();
            if (isNoteVertex(v)) {
                checkVertexProperty(v, SemanticSynchrony.PropertyKeys.ID, "id");
                checkVertexProperty(v, SemanticSynchrony.PropertyKeys.SOURCE, "source");
                checkVertexProperty(v, SemanticSynchrony.PropertyKeys.WEIGHT, "weight");
                checkVertexProperty(v, SemanticSynchrony.PropertyKeys.CREATED, "created");
                checkVertexProperty(v, SemanticSynchrony.PropertyKeys.TITLE, "title");
            }
        }
    }

    private void checkVertexProperty(final Vertex v, final String propertyKey, final String name) {
        VertexProperty<?> prop = v.property(propertyKey);
        if (!prop.isPresent()) {
            String id = v.property(SemanticSynchrony.PropertyKeys.ID).value().toString();
            System.out.println("note " + id + " has null " + name);
        }
    }

    private boolean isNoteVertex(final Vertex v) {
        String label = v.label();
        return null != label && label.equals(SemanticSynchrony.VertexLabels.NOTE);
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return true;
    }
}
