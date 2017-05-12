package net.fortytwo.smsn.server.actions;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.vcs.VCSFormat;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.config.DataSource;
import net.fortytwo.smsn.git.SmSnGitRepository;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.File;
import java.io.IOException;
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
            //migrateIds(context);

            //findAnomalousAtoms(context);

            assignSources(context);
        } catch (Exception e) {
            throw new RequestProcessingException(e);
        }
    }

    private void assignSources(ActionContext context) throws IOException {
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            File dir = new File(source.getLocation());
            Preconditions.checkArgument(dir.exists() && dir.isDirectory());
            TopicGraph graph = context.getBrain().getTopicGraph();
            for (File file : dir.listFiles()) {
                if (VCSFormat.isAtomFile(file)) {
                    String id = file.getName();
                    Optional<Atom> opt = graph.getAtomById(id);
                    Preconditions.checkArgument(opt.isPresent());
                    opt.get().setSource(source.getName());
                }
            }
            //SmSnGitRepository repo = new SmSnGitRepository(context.getBrain(), source);

        }
    }

    private void migrateIds(final ActionContext context) {
        TopicGraph graph = context.getBrain().getTopicGraph();
        for (Atom a : graph.getAllAtoms()) {
            a.setId(SemanticSynchrony.migrateId(a.getId()));
        }
    }

    private void findAnomalousAtoms(final ActionContext context) {
        for (Atom a : context.getBrain().getTopicGraph().getAllAtoms()) {
            checkNotNull(a, Atom::getId, "id");
            checkNotNull(a, Atom::getSource, "source");
            checkNotNull(a, Atom::getWeight, "weight");
            checkNotNull(a, Atom::getCreated, "created");
            checkNotNull(a, Atom::getTitle, "title");
        }
    }

    private <T> void checkNotNull(final Atom a, final Function<Atom, T> accessor,  final String name) {
        T value = accessor.apply(a);
        if (null == value) {
            System.out.println("atom " + a.getId() + " has null " + name);
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
