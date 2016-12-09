package net.fortytwo.smsn.server.errors;

public class RequestProcessingException extends RuntimeException {
    public RequestProcessingException(String message) {
        super(message);
    }

    public RequestProcessingException(Throwable cause) {
        super(cause);
    }
}
