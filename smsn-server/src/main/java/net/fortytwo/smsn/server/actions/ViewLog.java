package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.util.TreeNodeConverter;
import net.fortytwo.smsn.git.RepositoryCollection;
import net.fortytwo.smsn.git.SmSnGitRepository;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.json.JSONObject;

import java.io.File;
import java.io.IOException;

public class ViewLog extends BasicViewAction {
    private File file;

    public File getFile() {
        return notNull(file);
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }

    @Override
    protected void performTransaction(final ActionContext context)
            throws RequestProcessingException, BadRequestException {

        super.performTransaction(context);

        try {
            RepositoryCollection repos = new RepositoryCollection(context.getBrain(), getFile());

            // Get history as old TreeNode<Link> from git repository
            net.fortytwo.smsn.brain.model.entities.TreeNode<net.fortytwo.smsn.brain.model.entities.Link> legacyView
                = repos.getHistory(createExampleLimits());

            // Convert to new immutable TreeNode
            net.fortytwo.smsn.brain.TreeNode view = TreeNodeConverter.fromTreeNodeLink(legacyView);

            // Serialize directly using new JSON printer
            JSONObject json = context.getTreeNodeJsonPrinter().toJson(view);
            context.getMap().put(Params.VIEW, json);
        } catch (IOException | GitAPIException e) {
            throw new RequestProcessingException(e);
        }
    }

    public static SmSnGitRepository.Limits createExampleLimits() {
        SmSnGitRepository.Limits limits = new SmSnGitRepository.Limits();
        limits.setMaxDiffsPerRepository(10);
        limits.setMaxFilesPerDiff(10);
        return limits;
    }
}