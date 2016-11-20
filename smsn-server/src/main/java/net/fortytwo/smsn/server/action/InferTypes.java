package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

public class InferTypes extends Action {

    @Override
    public String getName() {
        return "infer-types";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        // do *not* reset the knowledge base.  Build upon inference performed in previous iterations
        KnowledgeBase kb = p.getBrain().getKnowledgeBase();
        //kb.reset();

        long timeBefore = System.currentTimeMillis();

        // note: multiple (typically four) invocations are required before the knowledge base is ready for RDF export
        kb.inferClasses(null, null);

        long timeAfter = System.currentTimeMillis();
        logger.info("completed type inference in " + (timeAfter - timeBefore) + "ms");
    }

    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service;
        // this operation only affects future calls which do read
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }
}
