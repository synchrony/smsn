package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.git.RepositoryCollection;
import net.fortytwo.smsn.git.SmSnGitRepository;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.eclipse.jgit.api.errors.GitAPIException;

import javax.validation.constraints.NotNull;
import java.io.File;
import java.io.IOException;

public class ViewLog extends BasicViewAction {
    @NotNull
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
            Note view = repos.getHistory(createExampleLimits());
            addView(view, context);
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