package net.fortytwo.smsn.server;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

/**
 * Read-only access to the exploded kg-* entity files: one JSON file per
 * Hydra entity, keyed by atom id. Maintains a cached index of which atoms
 * have entity records, so that views can mark them cheaply.
 *
 * The files are a periodic snapshot of the knowledge graph; getVintage()
 * reports how old, and clients should surface it.
 */
public final class HydraEntityStore {

    private static final String DATA_DIR_PROPERTY = "smsn.hydra.data";
    private static final String DATA_DIR_ENV = "SMSN_HYDRA_DATA";
    private static final String DEFAULT_DATA_DIR = "projects/github/joshsh/joshkg/data/kg";

    private static final long REFRESH_INTERVAL_MS = 60_000;

    // atom ids are alphanumeric; exclude path separators and dots
    public static final Pattern SAFE_ATOM_ID = Pattern.compile("[A-Za-z0-9@&_-]+");

    private static volatile Snapshot snapshot;
    private static volatile String cachedVintage;

    private static final class Snapshot {
        final List<File> sourceDirs;
        final Set<String> entityIds;
        final long loadedAt;

        Snapshot(List<File> sourceDirs, Set<String> entityIds, long loadedAt) {
            this.sourceDirs = sourceDirs;
            this.entityIds = entityIds;
            this.loadedAt = loadedAt;
        }
    }

    private HydraEntityStore() {
    }

    /**
     * The configured data directory, or null if it does not exist.
     */
    public static File getDataDir() {
        String path = System.getProperty(DATA_DIR_PROPERTY);
        if (null == path) {
            path = System.getenv(DATA_DIR_ENV);
        }
        if (null == path) {
            path = new File(System.getProperty("user.home"), DEFAULT_DATA_DIR).getAbsolutePath();
        }
        File dir = new File(path);
        return dir.isDirectory() ? dir : null;
    }

    public static String describeDataDirConfig() {
        return "set -D" + DATA_DIR_PROPERTY + " or " + DATA_DIR_ENV;
    }

    /**
     * Whether the given atom has an entity record of its own.
     */
    public static boolean hasEntity(final String atomId) {
        Snapshot s = currentSnapshot();
        return null != s && s.entityIds.contains(atomId);
    }

    /**
     * Load the entity record for the given atom, or null if the atom has none.
     * Returns null (rather than failing) when the store is unavailable.
     */
    public static JSONObject find(final String atomId) {
        if (!SAFE_ATOM_ID.matcher(atomId).matches()) {
            return null;
        }
        Snapshot s = currentSnapshot();
        if (null == s || !s.entityIds.contains(atomId)) {
            return null;
        }
        for (File sourceDir : s.sourceDirs) {
            File entityFile = new File(sourceDir, atomId + ".json");
            if (entityFile.isFile()) {
                try {
                    String content = new String(Files.readAllBytes(entityFile.toPath()), StandardCharsets.UTF_8);
                    return new JSONObject(content);
                } catch (IOException e) {
                    return null;
                }
            }
        }
        return null;
    }

    /**
     * Recursively mark tree-view nodes whose atoms have entity records with
     * "hasEntity": true. A no-op if the store is unavailable.
     */
    public static void annotateTree(final JSONObject viewNode) {
        Snapshot s = currentSnapshot();
        if (null != s) {
            annotate(viewNode, s.entityIds);
        }
    }

    private static void annotate(final JSONObject node, final Set<String> entityIds) {
        if (null == node) {
            return;
        }
        Object id = node.opt("id");
        if (id instanceof String && entityIds.contains(id)) {
            node.put("hasEntity", true);
        }
        Object children = node.opt("children");
        if (children instanceof JSONArray) {
            JSONArray array = (JSONArray) children;
            for (int i = 0; i < array.length(); i++) {
                Object child = array.opt(i);
                if (child instanceof JSONObject) {
                    annotate((JSONObject) child, entityIds);
                }
            }
        }
    }

    private static Snapshot currentSnapshot() {
        Snapshot s = snapshot;
        long now = System.currentTimeMillis();
        if (null == s || now - s.loadedAt > REFRESH_INTERVAL_MS) {
            synchronized (HydraEntityStore.class) {
                s = snapshot;
                if (null == s || now - s.loadedAt > REFRESH_INTERVAL_MS) {
                    s = loadSnapshot(now);
                    snapshot = s;
                }
            }
        }
        return s;
    }

    private static Snapshot loadSnapshot(final long now) {
        File dataDir = getDataDir();
        if (null == dataDir) {
            return null;
        }
        List<File> sourceDirs = new ArrayList<>();
        Set<String> entityIds = new HashSet<>();
        File[] kgDirs = dataDir.listFiles(f -> f.isDirectory() && f.getName().startsWith("kg-"));
        if (null != kgDirs) {
            for (File kgDir : kgDirs) {
                File atomsDir = new File(kgDir, "atoms");
                if (!atomsDir.isDirectory()) {
                    continue;
                }
                sourceDirs.add(atomsDir);
                String[] names = atomsDir.list((d, name) -> name.endsWith(".json"));
                if (null != names) {
                    for (String name : names) {
                        entityIds.add(name.substring(0, name.length() - ".json".length()));
                    }
                }
            }
        }
        return new Snapshot(sourceDirs, entityIds, now);
    }

    /**
     * Best-effort vintage of the exploded files: the date of the last commit
     * touching the data directory.
     */
    public static String getVintage() {
        String vintage = cachedVintage;
        if (null == vintage) {
            File dataDir = getDataDir();
            vintage = (null == dataDir) ? "unknown" : lookupVintage(dataDir);
            cachedVintage = vintage;
        }
        return vintage;
    }

    private static String lookupVintage(final File dataDir) {
        try {
            Process process = new ProcessBuilder(
                    "git", "-C", dataDir.getAbsolutePath(), "log", "-1", "--format=%cs", "--", ".")
                    .redirectErrorStream(true)
                    .start();
            String line;
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
                line = reader.readLine();
            }
            if (!process.waitFor(10, TimeUnit.SECONDS)) {
                process.destroyForcibly();
                return "unknown";
            }
            return (0 == process.exitValue() && null != line && !line.isEmpty()) ? line.trim() : "unknown";
        } catch (IOException e) {
            return "unknown";
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return "unknown";
        }
    }
}
