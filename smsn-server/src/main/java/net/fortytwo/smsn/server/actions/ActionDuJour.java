package net.fortytwo.smsn.server.actions;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.vcs.VCSFormat;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.pg.PGNote;
import net.fortytwo.smsn.config.Configuration;
import net.fortytwo.smsn.config.DataSource;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.apache.tinkerpop.gremlin.structure.Property;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
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
        for (Note note : context.getBrain().getTopicGraph().getAllNotes()) {
            Vertex v = ((PGNote) note).asVertex();
            VertexProperty<String> prop = v.property("page");
            if (prop.isPresent()) {
                String text = prop.value();
                prop.remove();
                v.property(SemanticSynchrony.PropertyKeys.TEXT, text);
            }
        }
    }

    private void sharabilityToSource(final ActionContext context) {
        for (Note note : context.getBrain().getTopicGraph().getAllNotes()) {
            Property<String> source1 = ((PGNote) note).asVertex().property("source");
            if (!source1.isPresent()) {
                System.out.println("note " + Note.getId(note) + " has no source. Title: " + Note.getTitle(note));
                Property<Float> sharability = ((PGNote) note).asVertex().property("sharability");
                if (sharability.isPresent()) {
                    System.out.println("\tsharability: " + sharability.value());
                } else {
                    System.out.println("\tno sharability");
                }

                String source = sourceForSharability(note);
                if (null != source) {
                    Note.setSource(note, source);
                }
            }
        }
    }

    private String sourceForSharability(final Note note) {
        Property<Float> sharability = ((PGNote) note).asVertex().property("sharability");
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
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            File dir = new File(source.getLocation());
            Preconditions.checkArgument(dir.exists() && dir.isDirectory());
            TopicGraph graph = context.getBrain().getTopicGraph();
            for (File file : dir.listFiles()) {
                if (VCSFormat.isSmSnFile(file)) {
                    AtomId id = new AtomId(file.getName());
                    Optional<Note> opt = graph.getNoteById(id);
                    Preconditions.checkArgument(opt.isPresent());
                    Note.setSource(opt.get(), source.getName());
                }
            }
            //SmSnGitRepository repo = new SmSnGitRepository(context.getBrain(), source);

        }
    }

    private void migrateIds(final ActionContext context) {
        TopicGraph graph = context.getBrain().getTopicGraph();
        for (Note a : graph.getAllNotes()) {
            Note.setId(a, SemanticSynchrony.migrateId(Note.getId(a)));
        }
    }

    private void findAnomalousNotes(final ActionContext context) {
        for (Note a : context.getBrain().getTopicGraph().getAllNotes()) {
            checkNotNull(a, Note::getId, "id");
            checkNotNull(a, Note::getSource, "source");
            checkNotNull(a, Note::getWeight, "weight");
            checkNotNull(a, Note::getCreated, "created");
            checkNotNull(a, Note::getTitle, "title");
        }
    }

    private <T> void checkNotNull(final Note a, final Function<Note, T> accessor, final String name) {
        T value = accessor.apply(a);
        if (null == value) {
            System.out.println("note " + Note.getId(a) + " has null " + name);
        }
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
