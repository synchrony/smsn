package net.fortytwo.smsn.brain.model;

import com.tinkerpop.blueprints.Vertex;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface GraphEntity {
    Vertex asVertex();
}
