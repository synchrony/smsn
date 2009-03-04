package net.fortytwo.myotherbrain.model;

import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:04:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class MOB {
    public static final String NAMESPACE = "http://fortytwo.net/2009/01/myotherbrain#";

    public static final URI
            ASSOCIATION,
            FIRSTCLASSITEM,
            OBJECT,
            SUBJECT,
            WEIGHT;

    static {
        ValueFactory factory = ValueFactoryImpl.getInstance();
        ASSOCIATION = factory.createURI(NAMESPACE, "Association");
        FIRSTCLASSITEM = factory.createURI(NAMESPACE, "FirstClassItem");
        OBJECT = factory.createURI(NAMESPACE, "object");
        SUBJECT = factory.createURI(NAMESPACE, "subject");
        WEIGHT = factory.createURI(NAMESPACE, "weight");
    }
}
