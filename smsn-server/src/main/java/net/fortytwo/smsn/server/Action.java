package net.fortytwo.smsn.server;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.History;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonParser;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonPrinter;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "action")
@JsonIgnoreProperties(ignoreUnknown = true)
public abstract class Action {

    protected static final Logger logger = Logger.getLogger(Action.class.getName());

    protected static final int MAX_VIEW_HEIGHT = 7;

    protected static final AtomId CREATE_NEW_NOTE = new AtomId("create-new-note");

    private static final History history = new History();

    // Request ID for matching responses to requests (echoed back to client)
    private String requestId;

    public String getRequestId() {
        return requestId;
    }

    public void setRequestId(String requestId) {
        this.requestId = requestId;
    }

    // override in subclasses
    protected void performTransaction(final ActionContext context)
            throws BadRequestException, RequestProcessingException {}

    protected abstract boolean doesRead();

    protected abstract boolean doesWrite();

    /**
     * Handle request using file-based repository.
     */
    public void handleRequest(final ActionContext context) {
        long before = System.currentTimeMillis();

        setTitle(context, "[no title]");

        try {
            performTransaction(context);
            context.getRepository().commit();
        } catch (Exception e) {
            e.printStackTrace(System.err);
            context.getRepository().rollback();
            throw new RequestProcessingException(e);
        }

        long after = System.currentTimeMillis();

        // Echo back the request ID for matching responses to requests
        if (requestId != null) {
            context.getMap().put(Params.REQUEST_ID, requestId);
        }

        SemanticSynchrony.getLogger().log(Level.INFO, "completed " + getClass().getSimpleName()
                + " action in " + (after - before) + " ms");

        logActivity(context);
    }

    protected void addToHistory(final AtomId rootId) {
        history.visit(rootId);
    }

    protected List<Atom> getHistory(final net.fortytwo.smsn.brain.repository.AtomRepositoryInterface repository,
                                     final Filter filter) {
        return history.getHistory(100, repository, filter);
    }

    private void logActivity(final ActionContext context) {
        ActivityLog activityLog = context.getActivityLog();
        if (null != activityLog) {
            activityLog.flush();
        }
    }

    protected void setTitle(final ActionContext context, final String title) {
        context.getMap().put(Params.VIEW_TITLE, title);
    }

    protected <T> T notNull(T object) {
        Preconditions.checkNotNull(object, "action is missing a required field");
        return object;
    }
}
