package net.fortytwo.myotherbrain.model;

import net.fortytwo.myotherbrain.update.Quotas;
import org.openrdf.elmo.ElmoManagerFactory;
import org.openrdf.elmo.ElmoModule;
import org.openrdf.elmo.sesame.SesameManagerFactory;
import org.openrdf.query.QueryLanguage;
import org.openrdf.repository.Repository;
import org.openrdf.sail.Sail;

import javax.xml.namespace.QName;

/**
 * Author: josh
 * Date: Jun 29, 2009
 * Time: 2:14:16 PM
 */
public class MOBModel {
    private final SesameManagerFactory elmoManagerFactory;
    private final Sail sail;

    // TODO: this belongs not so much in a model as in a session
    private final Quotas quotas;

    public MOBModel(final Sail sail,
                    final ElmoModule baseModule,
                    final Repository repository,
                    final QName writableGraph) {
        this.sail = sail;

        ElmoModule elmoModule = new ElmoModule();
        if (null != writableGraph) {
            elmoModule.setGraph(writableGraph);
        }
        // The graphs of the included module will be query-only.
        elmoModule.includeModule(baseModule);

        elmoManagerFactory
                = new SesameManagerFactory(elmoModule, repository);
        elmoManagerFactory.setQueryLanguage(QueryLanguage.SPARQL);

        quotas = new Quotas();
    }

    public MOBModelConnection createConnection() {
        return new MOBModelConnection(this);
    }

    public ElmoManagerFactory getElmoManagerFactory() {
        return elmoManagerFactory;
    }

    public Quotas getQuotas() {
        return quotas;
    }

    public Sail getSail() {
        // FIXME: this sail does not have the appropriate access restrictions
        return sail;
    }
}
