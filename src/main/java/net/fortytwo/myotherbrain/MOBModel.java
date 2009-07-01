package net.fortytwo.myotherbrain;

import org.openrdf.elmo.ElmoModule;
import org.openrdf.elmo.sesame.SesameManagerFactory;
import org.openrdf.repository.Repository;
import org.openrdf.model.URI;
import org.openrdf.query.QueryLanguage;

import javax.xml.namespace.QName;

/**
 * Author: josh
 * Date: Jun 29, 2009
 * Time: 2:14:16 PM
 */
public class MOBModel {
    private final ElmoModule elmoModule;
    private final SesameManagerFactory elmoManagerFactory;

    public MOBModel(final ElmoModule baseModule,
                    final Repository repository,
                    final URI writableGraph) {
        elmoModule = new ElmoModule();
        if (null != writableGraph) {
            elmoModule.setGraph(uriToQName(writableGraph));
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

    private QName uriToQName(final URI uri) {
        return new QName(uri.toString());
    }
}
