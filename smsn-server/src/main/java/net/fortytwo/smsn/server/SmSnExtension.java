package net.fortytwo.smsn.server;

import net.fortytwo.smsn.server.action.BroadcastRDF;
import net.fortytwo.smsn.server.action.EvaluateRippleQuery;
import net.fortytwo.smsn.server.action.EvaluateTextSearch;
import net.fortytwo.smsn.server.action.FindDuplicates;
import net.fortytwo.smsn.server.action.FindIsolatedAtoms;
import net.fortytwo.smsn.server.action.FindRoots;
import net.fortytwo.smsn.server.action.GetEvents;
import net.fortytwo.smsn.server.action.GetHistory;
import net.fortytwo.smsn.server.action.GetPriorities;
import net.fortytwo.smsn.server.action.GetView;
import net.fortytwo.smsn.server.action.InferTypes;
import net.fortytwo.smsn.server.action.PushEvent;
import net.fortytwo.smsn.server.action.ReadGraph;
import net.fortytwo.smsn.server.action.RemoveIsolatedAtoms;
import net.fortytwo.smsn.server.action.SetProperties;
import net.fortytwo.smsn.server.action.UpdateView;
import net.fortytwo.smsn.server.action.WriteGraph;
import net.fortytwo.smsn.util.TypedProperties;
import org.apache.tinkerpop.gremlin.groovy.plugin.AbstractGremlinPlugin;
import org.apache.tinkerpop.gremlin.groovy.plugin.IllegalEnvironmentException;
import org.apache.tinkerpop.gremlin.groovy.plugin.PluginAcceptor;
import org.apache.tinkerpop.gremlin.groovy.plugin.PluginInitializationException;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class SmSnExtension extends AbstractGremlinPlugin {
    protected static final Logger logger = Logger.getLogger(SmSnExtension.class.getName());

    private final Map<String, Action> extensionsByName;

    public SmSnExtension() throws InterruptedException, IOException, TypedProperties.PropertyException {
        extensionsByName = new HashMap<>();

        // TODO: replace with a classloader
        add(new BroadcastRDF());
        add(new FindDuplicates());
        add(new WriteGraph());
        add(new FindIsolatedAtoms());
        add(new FindRoots());
        add(new GetEvents());
        add(new GetHistory());
        add(new ReadGraph());
        add(new InferTypes());
        add(new GetPriorities());
        add(new PushEvent());
        add(new RemoveIsolatedAtoms());
        add(new EvaluateRippleQuery());
        add(new EvaluateTextSearch());
        add(new SetProperties());
        add(new UpdateView());
        add(new GetView());
    }

    @Override
    public void afterPluginTo(PluginAcceptor pluginAcceptor) throws IllegalEnvironmentException, PluginInitializationException {
        // nothing to do?
    }

    private void add(Action extension) {
        extensionsByName.put(extension.getName(), extension);
    }

    /*
    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    public ExtensionResponse handleRequest(@RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {
        try {
            return handleRequestInternal(graph, request);
        } catch (Exception e) {
            logger.log(Level.WARNING, "unclassified exception", e);
            return ExtensionResponse.error(e);
        }
    }

    private ExtensionResponse handleRequestInternal(Graph graph, String request) {
        JSONObject json;
        String action;
        try {
            json = new JSONObject(request);
            action = json.getString(Params.ACTION);
        } catch (JSONException e) {
            return ExtensionResponse.badRequest(e.getMessage(), null);
        }

        if (null == action) {
            return ExtensionResponse.badRequest("missing '" + Params.ACTION + "' parameter", null);
        }

        Action.RequestParams p = Action.createParams((KeyIndexableGraph) graph);

        Action extension = extensionsByName.get(action);
        if (null == extension) {
            return ExtensionResponse.error("unsupported action: " + action);
        }

        SemanticSynchrony.logInfo("SmSn " + action);

        try {
            extension.parseRequest(json, p);
        } catch (JSONException e) {
            return ExtensionResponse.badRequest(e.getMessage(), null);
        } catch (BadRequestException e) {
            return ExtensionResponse.badRequest(e.getMessage(), null);
        }

        try {
            extension.handleRequestInternal(p);
        } catch (AuthorizationException e) {
            return ExtensionResponse.error(e.getMessage()); // TODO: not auth
        } catch (BadRequestException e) {
            return ExtensionResponse.error(e.getMessage());
        } catch (RequestProcessingException e) {
            logger.log(Level.WARNING, "internal error", e);
            return ExtensionResponse.error(e);
        } catch (InvalidUpdateException e) {
            return ExtensionResponse.error(e.getMessage());
        } catch (InvalidGraphException e) {
            logger.log(Level.WARNING, "invalid graph", e);
            return ExtensionResponse.error(e);
        }

        return ExtensionResponse.ok(p.map);
    }
    */

    @Override
    public String getName() {
        return "smsn";
    }
}
