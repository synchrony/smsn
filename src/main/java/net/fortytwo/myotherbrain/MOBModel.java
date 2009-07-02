package net.fortytwo.myotherbrain;

import org.openrdf.elmo.ElmoModule;
import org.openrdf.elmo.sesame.SesameManagerFactory;
import org.openrdf.query.QueryLanguage;
import org.openrdf.repository.Repository;

import javax.xml.namespace.QName;

/**
 * Author: josh
 * Date: Jun 29, 2009
 * Time: 2:14:16 PM
 */
public class MOBModel {
    private final SesameManagerFactory elmoManagerFactory;

    public MOBModel(final ElmoModule baseModule,
                    final Repository repository,
                    final QName writableGraph) {
        ElmoModule elmoModule = new ElmoModule();
        if (null != writableGraph) {
            elmoModule.setGraph(writableGraph);
        }
        // The graphs of the included module will be read-only.
        elmoModule.includeModule(baseModule);

        elmoManagerFactory
                = new SesameManagerFactory(elmoModule, repository);
        elmoManagerFactory.setQueryLanguage(QueryLanguage.SPARQL);
    }

    public MOBModelConnection createConnection() {
        return new MOBModelConnection(elmoManagerFactory);
    }
}
