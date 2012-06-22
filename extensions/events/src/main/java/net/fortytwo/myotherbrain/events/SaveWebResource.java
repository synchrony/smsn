package net.fortytwo.myotherbrain.events;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.HttpMethod;
import com.tinkerpop.rexster.extension.RexsterContext;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "events", name = "save-web-resource")
public class SaveWebResource extends AbstractRexsterExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "an extension for caching a web resource")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "content") String content,
                                           @ExtensionRequestParameter(name = "id", description = "id (e.g. URL hash) of the to-be-cached document") String id,
                                           @ExtensionRequestParameter(name = "url", description = "URL of the to-be-cached document") String url,
                                           @ExtensionRequestParameter(name = "accept", description = "HTTP Accept header for requesting the document") String accept) {
        return ExtensionResponse.error("not yet implemented");
    }
}