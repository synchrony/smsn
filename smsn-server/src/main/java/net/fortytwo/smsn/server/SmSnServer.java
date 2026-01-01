package net.fortytwo.smsn.server;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonParser;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonPrinter;
import net.fortytwo.smsn.brain.io.wiki.TreeNodeWikiParser;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.repository.AtomRepositoryInterface;
import net.fortytwo.smsn.brain.repository.FileBasedAtomRepository;
import net.fortytwo.smsn.brain.repository.FileBasedTopicGraph;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.websocket.server.config.JettyWebSocketServletContainerInitializer;
import org.json.JSONObject;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.SQLException;
import java.time.Duration;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Standalone WebSocket server for SmSn, without TinkerPop/Gremlin Server dependencies.
 * Uses FileBasedAtomRepository for storage and Jetty for WebSocket communication.
 */
public class SmSnServer {
    private static final Logger logger = Logger.getLogger(SmSnServer.class.getName());

    private static final int DEFAULT_PORT = 8182;
    private static final String DEFAULT_HOST = "0.0.0.0";
    // Use /gremlin path for compatibility with existing Web UI
    private static final String WEBSOCKET_PATH = "/gremlin";

    private final Server server;
    private final FileBasedAtomRepository repository;
    private final TopicGraph topicGraph;
    private final ObjectMapper objectMapper;
    private final ActivityLog activityLog;

    public SmSnServer(int port, String host) throws IOException, SQLException {
        this.objectMapper = new ObjectMapper();

        // Initialize file-based repository
        File indexDir = new File(SemanticSynchrony.getConfiguration().getIndexDirectory());
        logger.info("Initializing file-based repository with index directory: " + indexDir.getAbsolutePath());
        this.repository = new FileBasedAtomRepository(indexDir);
        this.repository.initialize();

        // Create TopicGraph adapter for VCS export and other operations
        this.topicGraph = new FileBasedTopicGraph(repository);

        // Create activity log directly (no Brain needed for standalone mode)
        this.activityLog = createActivityLog();

        // Create Jetty server
        this.server = new Server();
        ServerConnector connector = new ServerConnector(server);
        connector.setHost(host);
        connector.setPort(port);
        server.addConnector(connector);

        // Set up WebSocket
        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setContextPath("/");
        server.setHandler(context);

        JettyWebSocketServletContainerInitializer.configure(context, (servletContext, wsContainer) -> {
            wsContainer.setIdleTimeout(Duration.ofMinutes(10));
            wsContainer.setMaxTextMessageSize(65536 * 10);
            wsContainer.addMapping(WEBSOCKET_PATH, (req, resp) -> new SmSnWebSocket(this));
        });

        logger.info("SmSn Server configured on " + host + ":" + port + WEBSOCKET_PATH);
    }

    private ActivityLog createActivityLog() {
        String filePath = SemanticSynchrony.getConfiguration().getActivityLog();
        if (filePath == null) {
            logger.warning("No activity log specified in configuration");
            return null;
        }
        try {
            File logFile = new File(filePath);
            logFile.getParentFile().mkdirs();
            return new ActivityLog(new FileWriter(logFile, true));
        } catch (IOException e) {
            logger.warning("Could not create activity log: " + e.getMessage());
            return null;
        }
    }

    public ActivityLog getActivityLog() {
        return activityLog;
    }

    public void start() throws Exception {
        server.start();
        logger.info("SmSn Server started");
    }

    public void stop() throws Exception {
        server.stop();
        repository.close();
        logger.info("SmSn Server stopped");
    }

    public void join() throws InterruptedException {
        server.join();
    }

    /**
     * Handle a request from a WebSocket client.
     */
    public String handleRequest(String requestJson) {
        try {
            Action action = objectMapper.readValue(requestJson, Action.class);
            ActionContext context = createContext();

            action.handleRequestStandalone(context);

            JSONObject response = new JSONObject(context.getMap());
            return response.toString();
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error handling request", e);
            JSONObject error = new JSONObject();
            error.put("error", e.getMessage());
            return error.toString();
        }
    }

    private ActionContext createContext() {
        ActionContext context = new ActionContext();
        context.setMap(new HashMap<>());
        context.setRepository(repository);
        context.setTopicGraph(topicGraph);
        context.setActivityLog(activityLog);
        context.setTreeNodeWikiParser(new TreeNodeWikiParser());
        context.setTreeNodeJsonParser(new TreeNodeJsonParser());
        context.setTreeNodeJsonPrinter(new TreeNodeJsonPrinter());
        return context;
    }

    public AtomRepositoryInterface getRepository() {
        return repository;
    }

    public static void main(String[] args) {
        int port = DEFAULT_PORT;
        String host = DEFAULT_HOST;

        // Parse command line arguments
        for (int i = 0; i < args.length; i++) {
            if ("-p".equals(args[i]) || "--port".equals(args[i])) {
                if (i + 1 < args.length) {
                    port = Integer.parseInt(args[++i]);
                }
            } else if ("-h".equals(args[i]) || "--host".equals(args[i])) {
                if (i + 1 < args.length) {
                    host = args[++i];
                }
            } else if ("-c".equals(args[i]) || "--config".equals(args[i])) {
                if (i + 1 < args.length) {
                    String configFile = args[++i];
                    try {
                        try (java.io.FileInputStream fis = new java.io.FileInputStream(configFile)) {
                            SemanticSynchrony.readConfigurationYaml(fis);
                        }
                        logger.info("Loaded configuration from: " + configFile);
                    } catch (IOException e) {
                        logger.severe("Failed to load configuration: " + e.getMessage());
                        System.exit(1);
                    }
                }
            } else if ("--help".equals(args[i])) {
                printUsage();
                System.exit(0);
            }
        }

        try {
            SmSnServer server = new SmSnServer(port, host);
            server.start();

            // Add shutdown hook
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                try {
                    server.stop();
                } catch (Exception e) {
                    logger.warning("Error during shutdown: " + e.getMessage());
                }
            }));

            server.join();
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Failed to start server", e);
            System.exit(1);
        }
    }

    private static void printUsage() {
        System.out.println("Usage: java -jar smsn-server.jar [options]");
        System.out.println("Options:");
        System.out.println("  -p, --port PORT     Server port (default: " + DEFAULT_PORT + ")");
        System.out.println("  -h, --host HOST     Server host (default: " + DEFAULT_HOST + ")");
        System.out.println("  -c, --config FILE   Configuration file (smsn.yaml)");
        System.out.println("      --help          Show this help message");
    }
}
