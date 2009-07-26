package net.fortytwo.myotherbrain.query;

/**
 * Author: josh
 * Date: Jul 14, 2009
 * Time: 6:57:54 PM
 */
public interface Handler<T, E extends Exception> {
    boolean handle(T t) throws E;
}
