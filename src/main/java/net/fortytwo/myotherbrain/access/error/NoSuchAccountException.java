package net.fortytwo.myotherbrain.access.error;

/**
 * Author: josh
 * Date: Jul 1, 2009
 * Time: 10:46:26 AM
 */
public class NoSuchAccountException extends Exception {
    public NoSuchAccountException(final String userName) {
        super("there is no account with user name '" + userName + "'");
    }
}
