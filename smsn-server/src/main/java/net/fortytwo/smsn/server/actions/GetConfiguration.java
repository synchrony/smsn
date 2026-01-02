package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.config.Configuration;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class GetConfiguration extends Action {
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    protected void performTransaction(final ActionContext context)
            throws RequestProcessingException, BadRequestException {
        Configuration config = SemanticSynchrony.getConfiguration();

        context.getMap().put(Params.CONFIGURATION, toJsonString(config));
    }

    private <T> String toJsonString(final T object) {
        try {
            return objectMapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException();
        }
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
