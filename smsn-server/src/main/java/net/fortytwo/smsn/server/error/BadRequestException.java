package net.fortytwo.smsn.server.error;

public class BadRequestException extends Exception {
    public BadRequestException(String message) {
        super(message);
    }
}
