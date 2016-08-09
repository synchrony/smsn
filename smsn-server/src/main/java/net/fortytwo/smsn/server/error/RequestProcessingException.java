package net.fortytwo.smsn.server.error;

public class RequestProcessingException extends Exception {
    public RequestProcessingException(String message) {
        super(message);
    }

    public RequestProcessingException(Throwable cause) {
        super(cause);
    }
}
