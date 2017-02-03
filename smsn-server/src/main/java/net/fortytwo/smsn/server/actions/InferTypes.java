package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

public class InferTypes extends Action {

    @Override
    public void parseRequest(final RequestParams params) {
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        // do *not* reset the knowledge base.  Build upon inference performed in previous iterations
        KnowledgeBase kb = params.getBrain().getKnowledgeBase();
        //kb.reset();

        long timeBefore = System.currentTimeMillis();

        // note: multiple (typically four) invocations are required before the knowledge base is ready for RDF export
        kb.inferClasses(null, null);

        long timeAfter = System.currentTimeMillis();
        logger.info("completed type inference in " + (timeAfter - timeBefore) + "ms");
    }

    @Override
    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service;
        // this operation only affects future calls which do read
        return false;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
