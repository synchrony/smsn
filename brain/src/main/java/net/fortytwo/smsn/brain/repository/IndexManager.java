package net.fortytwo.smsn.brain.repository;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.repository.Sortable;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;

/**
 * Manages SQLite and Lucene indices for file-based atom storage.
 *
 * SQLite handles:
 * - Fast ID lookups
 * - Shortcut lookups (exact match)
 * - Child relationship queries
 * - Atom metadata (source, created, weight, priority)
 *
 * Lucene handles:
 * - Full-text search on titles
 * - Acronym search
 */
public class IndexManager implements AutoCloseable {
    private static final Logger logger = SemanticSynchrony.getLogger();
    // Lucene returns top N results by native score before custom scoring is applied.
    // For wildcard queries, all matches have equal native scores, so results are
    // returned in document order. We need a high enough limit to capture matches
    // from all sources before the repository applies weight-based scoring.
    private static final int MAX_SEARCH_RESULTS = 5000;

    private static final String FIELD_ID = "id";
    private static final String FIELD_TITLE = "title";
    private static final String FIELD_ACRONYM = "acronym";

    private final File indexDirectory;
    private final Connection sqliteConnection;
    private final Directory luceneDirectory;
    private IndexWriter luceneWriter;
    private DirectoryReader luceneReader;
    private IndexSearcher luceneSearcher;
    private final StandardAnalyzer analyzer;
    private boolean luceneNeedsRebuild = false;

    public IndexManager(File indexDirectory) throws IOException, SQLException {
        this.indexDirectory = indexDirectory;
        if (!indexDirectory.exists()) {
            indexDirectory.mkdirs();
        }

        // Initialize SQLite with performance optimizations
        File sqliteFile = new File(indexDirectory, "index.db");
        String jdbcUrl = "jdbc:sqlite:" + sqliteFile.getAbsolutePath();
        this.sqliteConnection = DriverManager.getConnection(jdbcUrl);

        // Enable WAL mode for better concurrent performance
        try (Statement stmt = sqliteConnection.createStatement()) {
            stmt.execute("PRAGMA journal_mode=WAL");
            stmt.execute("PRAGMA synchronous=NORMAL");
            stmt.execute("PRAGMA cache_size=10000");
        }

        initializeSqliteTables();

        // Initialize Lucene
        File luceneDir = new File(indexDirectory, "lucene");
        Path lucenePath = luceneDir.toPath();
        this.luceneDirectory = FSDirectory.open(lucenePath);
        this.analyzer = new StandardAnalyzer();

        // Check if existing index needs to be deleted due to format incompatibility
        boolean needsRebuild = false;
        if (luceneDir.exists() && luceneDir.listFiles() != null && luceneDir.listFiles().length > 0) {
            try {
                // Try to open the existing index
                IndexWriterConfig testConfig = new IndexWriterConfig(analyzer);
                testConfig.setOpenMode(IndexWriterConfig.OpenMode.APPEND);
                IndexWriter testWriter = new IndexWriter(luceneDirectory, testConfig);
                testWriter.close();
            } catch (org.apache.lucene.index.IndexFormatTooOldException e) {
                logger.warning("Lucene index format is too old, will rebuild: " + e.getMessage());
                needsRebuild = true;
                luceneNeedsRebuild = true;
                // Delete the old index
                for (File f : luceneDir.listFiles()) {
                    f.delete();
                }
            }
        }

        IndexWriterConfig config = new IndexWriterConfig(analyzer);
        config.setOpenMode(needsRebuild ? IndexWriterConfig.OpenMode.CREATE : IndexWriterConfig.OpenMode.CREATE_OR_APPEND);
        config.setRAMBufferSizeMB(64.0); // Use more RAM for faster indexing
        this.luceneWriter = new IndexWriter(luceneDirectory, config);

        refreshLuceneReader();
    }

    private void initializeSqliteTables() throws SQLException {
        try (Statement stmt = sqliteConnection.createStatement()) {
            // Main atoms table
            stmt.execute(
                "CREATE TABLE IF NOT EXISTS atoms (" +
                "    id TEXT PRIMARY KEY," +
                "    source TEXT NOT NULL," +
                "    created INTEGER NOT NULL," +
                "    weight REAL NOT NULL," +
                "    priority REAL," +
                "    shortcut TEXT," +
                "    title TEXT" +
                ")"
            );

            // Children relationships
            stmt.execute(
                "CREATE TABLE IF NOT EXISTS children (" +
                "    parent_id TEXT NOT NULL," +
                "    child_id TEXT NOT NULL," +
                "    position INTEGER NOT NULL," +
                "    PRIMARY KEY (parent_id, position)" +
                ")"
            );

            // Indexes for common queries
            stmt.execute("CREATE INDEX IF NOT EXISTS idx_atoms_source ON atoms(source)");
            stmt.execute("CREATE INDEX IF NOT EXISTS idx_atoms_shortcut ON atoms(shortcut)");
            stmt.execute("CREATE INDEX IF NOT EXISTS idx_children_parent ON children(parent_id)");
            stmt.execute("CREATE INDEX IF NOT EXISTS idx_children_child ON children(child_id)");
        }
    }

    private void refreshLuceneReader() throws IOException {
        if (luceneReader == null) {
            luceneWriter.commit();
            luceneReader = DirectoryReader.open(luceneDirectory);
        } else {
            DirectoryReader newReader = DirectoryReader.openIfChanged(luceneReader);
            if (newReader != null) {
                luceneReader.close();
                luceneReader = newReader;
            }
        }
        luceneSearcher = new IndexSearcher(luceneReader);
    }

    /**
     * Index an atom in both SQLite and Lucene.
     */
    public void indexAtom(Atom atom) throws SQLException, IOException {
        indexAtomInSqlite(atom);
        indexAtomInLucene(atom, false);
    }

    /**
     * Bulk index many atoms efficiently.
     * Uses transactions and batch operations for much better performance.
     */
    public void indexAtomsBulk(List<Atom> atoms) throws SQLException, IOException {
        if (atoms.isEmpty()) {
            return;
        }

        long startTime = System.currentTimeMillis();
        logger.info("Bulk indexing " + atoms.size() + " atoms...");

        // Disable auto-commit for bulk insert
        boolean originalAutoCommit = sqliteConnection.getAutoCommit();
        sqliteConnection.setAutoCommit(false);

        try {
            // Batch insert atoms
            String atomSql = "INSERT OR REPLACE INTO atoms (id, source, created, weight, priority, shortcut, title) " +
                             "VALUES (?, ?, ?, ?, ?, ?, ?)";
            String childSql = "INSERT INTO children (parent_id, child_id, position) VALUES (?, ?, ?)";

            int atomCount = 0;
            int childCount = 0;

            try (PreparedStatement atomStmt = sqliteConnection.prepareStatement(atomSql);
                 PreparedStatement childStmt = sqliteConnection.prepareStatement(childSql)) {

                for (Atom atom : atoms) {
                    // Index atom metadata
                    atomStmt.setString(1, atom.id.value);
                    atomStmt.setString(2, atom.source.value);
                    atomStmt.setLong(3, atom.created.value);
                    atomStmt.setFloat(4, atom.weight.value);
                    atomStmt.setObject(5, atom.priority.isPresent() ? atom.priority.get().value : null);
                    atomStmt.setString(6, atom.shortcut.isPresent() ? atom.shortcut.get() : null);
                    atomStmt.setString(7, atom.title);
                    atomStmt.addBatch();
                    atomCount++;

                    // Index children
                    for (int i = 0; i < atom.children.size(); i++) {
                        childStmt.setString(1, atom.id.value);
                        childStmt.setString(2, atom.children.get(i).value);
                        childStmt.setInt(3, i);
                        childStmt.addBatch();
                        childCount++;
                    }

                    // Execute in batches of 1000 to avoid memory issues
                    if (atomCount % 1000 == 0) {
                        atomStmt.executeBatch();
                        childStmt.executeBatch();
                    }

                    // Index in Lucene (skip delete since this is fresh index)
                    indexAtomInLucene(atom, true);
                }

                // Execute remaining batches
                atomStmt.executeBatch();
                childStmt.executeBatch();
            }

            sqliteConnection.commit();

            long elapsed = System.currentTimeMillis() - startTime;
            logger.info("Bulk indexing completed: " + atomCount + " atoms, " + childCount +
                       " child relationships in " + elapsed + "ms");

        } catch (SQLException | IOException e) {
            sqliteConnection.rollback();
            throw e;
        } finally {
            sqliteConnection.setAutoCommit(originalAutoCommit);
        }
    }

    private void indexAtomInSqlite(Atom atom) throws SQLException {
        String sql = "INSERT OR REPLACE INTO atoms (id, source, created, weight, priority, shortcut, title) " +
                     "VALUES (?, ?, ?, ?, ?, ?, ?)";

        try (PreparedStatement stmt = sqliteConnection.prepareStatement(sql)) {
            stmt.setString(1, atom.id.value);
            stmt.setString(2, atom.source.value);
            stmt.setLong(3, atom.created.value);
            stmt.setFloat(4, atom.weight.value);
            stmt.setObject(5, atom.priority.isPresent() ? atom.priority.get().value : null);
            stmt.setString(6, atom.shortcut.isPresent() ? atom.shortcut.get() : null);
            stmt.setString(7, atom.title);
            stmt.executeUpdate();
        }
    }

    private void indexAtomInLucene(Atom atom, boolean skipDelete) throws IOException {
        // Only delete if updating (not during bulk initial indexing)
        if (!skipDelete) {
            luceneWriter.deleteDocuments(new Term(FIELD_ID, atom.id.value));
        }

        // Create new document
        Document doc = new Document();
        doc.add(new StringField(FIELD_ID, atom.id.value, Field.Store.YES));

        if (atom.title != null && !atom.title.isEmpty()) {
            doc.add(new TextField(FIELD_TITLE, atom.title, Field.Store.NO));
        }

        String acronym = generateAcronym(atom.title);
        if (acronym != null && !acronym.isEmpty()) {
            doc.add(new TextField(FIELD_ACRONYM, acronym, Field.Store.NO));
        }

        luceneWriter.addDocument(doc);
    }

    /**
     * Update children relationships in SQLite.
     */
    public void updateChildren(AtomId parentId, List<AtomId> childIds) throws SQLException {
        // Delete existing children
        try (PreparedStatement stmt = sqliteConnection.prepareStatement(
                "DELETE FROM children WHERE parent_id = ?")) {
            stmt.setString(1, parentId.value);
            stmt.executeUpdate();
        }

        // Insert new children
        if (!childIds.isEmpty()) {
            String sql = "INSERT INTO children (parent_id, child_id, position) VALUES (?, ?, ?)";
            try (PreparedStatement stmt = sqliteConnection.prepareStatement(sql)) {
                for (int i = 0; i < childIds.size(); i++) {
                    stmt.setString(1, parentId.value);
                    stmt.setString(2, childIds.get(i).value);
                    stmt.setInt(3, i);
                    stmt.addBatch();
                }
                stmt.executeBatch();
            }
        }
    }

    /**
     * Get children IDs from SQLite.
     */
    public List<AtomId> getChildrenIds(AtomId parentId) throws SQLException {
        List<AtomId> children = new ArrayList<>();
        String sql = "SELECT child_id FROM children WHERE parent_id = ? ORDER BY position";

        try (PreparedStatement stmt = sqliteConnection.prepareStatement(sql)) {
            stmt.setString(1, parentId.value);
            try (ResultSet rs = stmt.executeQuery()) {
                while (rs.next()) {
                    children.add(new AtomId(rs.getString("child_id")));
                }
            }
        }

        return children;
    }

    /**
     * Get parent IDs from SQLite.
     */
    public List<AtomId> getParentIds(AtomId childId) throws SQLException {
        List<AtomId> parents = new ArrayList<>();
        String sql = "SELECT DISTINCT parent_id FROM children WHERE child_id = ?";

        try (PreparedStatement stmt = sqliteConnection.prepareStatement(sql)) {
            stmt.setString(1, childId.value);
            try (ResultSet rs = stmt.executeQuery()) {
                while (rs.next()) {
                    parents.add(new AtomId(rs.getString("parent_id")));
                }
            }
        }

        return parents;
    }

    /**
     * Count parents of an atom.
     */
    public int countParents(AtomId atomId) throws SQLException {
        String sql = "SELECT COUNT(DISTINCT parent_id) FROM children WHERE child_id = ?";
        try (PreparedStatement stmt = sqliteConnection.prepareStatement(sql)) {
            stmt.setString(1, atomId.value);
            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    return rs.getInt(1);
                }
            }
        }
        return 0;
    }

    /**
     * Find atom by shortcut (exact match).
     */
    public Optional<AtomId> findByShortcut(String shortcut) throws SQLException {
        String sql = "SELECT id FROM atoms WHERE shortcut = ?";
        try (PreparedStatement stmt = sqliteConnection.prepareStatement(sql)) {
            stmt.setString(1, shortcut);
            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    return Optional.of(new AtomId(rs.getString("id")));
                }
            }
        }
        return Optional.empty();
    }

    /**
     * Search by title using Lucene full-text search.
     */
    public Iterator<Sortable<AtomId, Float>> searchByTitle(String queryString) throws IOException {
        return searchLucene(FIELD_TITLE, queryString);
    }

    /**
     * Search by acronym using Lucene.
     */
    public Iterator<Sortable<AtomId, Float>> searchByAcronym(String queryString) throws IOException {
        return searchLucene(FIELD_ACRONYM, queryString.toLowerCase());
    }

    private Iterator<Sortable<AtomId, Float>> searchLucene(String field, String queryString) throws IOException {
        refreshLuceneReader();

        List<Sortable<AtomId, Float>> results = new ArrayList<>();

        String sanitizedQuery = sanitizeQuery(queryString);
        if (sanitizedQuery.isEmpty()) {
            return results.iterator();
        }

        try {
            QueryParser parser = new QueryParser(field, analyzer);
            parser.setDefaultOperator(QueryParser.Operator.AND);
            parser.setAllowLeadingWildcard(true);
            Query query = parser.parse(sanitizedQuery);

            TopDocs topDocs = luceneSearcher.search(query, MAX_SEARCH_RESULTS);

            for (ScoreDoc scoreDoc : topDocs.scoreDocs) {
                Document doc = luceneSearcher.doc(scoreDoc.doc);
                String id = doc.get(FIELD_ID);
                if (id != null) {
                    results.add(new Sortable<>(new AtomId(id), scoreDoc.score));
                }
            }
        } catch (ParseException e) {
            logger.warning("Failed to parse query: " + queryString + " - " + e.getMessage());
        }

        return results.iterator();
    }

    /**
     * Sanitize a query string for Lucene.
     * Strips the "*term*" wrapper from smsn-mode queries and adds wildcards
     * to each term for substring matching.
     * Uses AND semantics - all terms must be present.
     */
    private String sanitizeQuery(String queryString) {
        if (queryString == null || queryString.isEmpty()) {
            return "";
        }

        String trimmed = queryString.trim();

        // Handle "*term*" format from smsn-mode - strip outer wildcards
        if (trimmed.startsWith("*") && trimmed.endsWith("*") && trimmed.length() > 2) {
            trimmed = trimmed.substring(1, trimmed.length() - 1).trim();
        }

        // Add wildcards to each term for substring matching
        // "log 25 purch" -> "*log* *25* *purch*"
        StringBuilder result = new StringBuilder();
        for (String term : trimmed.split("\\s+")) {
            if (!term.isEmpty()) {
                if (result.length() > 0) {
                    result.append(" ");
                }
                result.append("*").append(term).append("*");
            }
        }

        return result.toString();
    }

    /**
     * Get all atom IDs from SQLite.
     */
    public List<AtomId> getAllAtomIds() throws SQLException {
        List<AtomId> ids = new ArrayList<>();
        try (Statement stmt = sqliteConnection.createStatement();
             ResultSet rs = stmt.executeQuery("SELECT id FROM atoms")) {
            while (rs.next()) {
                ids.add(new AtomId(rs.getString("id")));
            }
        }
        return ids;
    }

    /**
     * Get all atom IDs for a specific source.
     */
    public List<AtomId> getAtomIdsBySource(String source) throws SQLException {
        List<AtomId> ids = new ArrayList<>();
        String sql = "SELECT id FROM atoms WHERE source = ?";
        try (PreparedStatement stmt = sqliteConnection.prepareStatement(sql)) {
            stmt.setString(1, source);
            try (ResultSet rs = stmt.executeQuery()) {
                while (rs.next()) {
                    ids.add(new AtomId(rs.getString("id")));
                }
            }
        }
        return ids;
    }

    /**
     * Remove an atom from the index.
     */
    public void removeAtom(AtomId id) throws SQLException, IOException {
        // Remove from SQLite
        try (PreparedStatement stmt = sqliteConnection.prepareStatement("DELETE FROM atoms WHERE id = ?")) {
            stmt.setString(1, id.value);
            stmt.executeUpdate();
        }

        try (PreparedStatement stmt = sqliteConnection.prepareStatement("DELETE FROM children WHERE parent_id = ?")) {
            stmt.setString(1, id.value);
            stmt.executeUpdate();
        }

        try (PreparedStatement stmt = sqliteConnection.prepareStatement("DELETE FROM children WHERE child_id = ?")) {
            stmt.setString(1, id.value);
            stmt.executeUpdate();
        }

        // Remove from Lucene
        luceneWriter.deleteDocuments(new Term(FIELD_ID, id.value));
    }

    /**
     * Check if an atom exists in the index.
     */
    public boolean exists(AtomId id) throws SQLException {
        String sql = "SELECT 1 FROM atoms WHERE id = ?";
        try (PreparedStatement stmt = sqliteConnection.prepareStatement(sql)) {
            stmt.setString(1, id.value);
            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next();
            }
        }
    }

    /**
     * Commit all pending changes to both SQLite and Lucene.
     */
    public void commit() throws IOException, SQLException {
        luceneWriter.commit();
        refreshLuceneReader();
        // SQLite auto-commits by default; for transactions we'd use setAutoCommit(false)
    }

    /**
     * Clear all indices. Use with caution!
     */
    public void clear() throws SQLException, IOException {
        try (Statement stmt = sqliteConnection.createStatement()) {
            stmt.execute("DELETE FROM atoms");
            stmt.execute("DELETE FROM children");
        }
        luceneWriter.deleteAll();
        luceneWriter.commit();
        refreshLuceneReader();
    }

    /**
     * Generate acronym from title (first letter of each word).
     */
    private String generateAcronym(String title) {
        if (title == null || title.length() > 100) {
            return null;
        }

        String clean = title.toLowerCase().replaceAll("[-_\t\n\r]", " ").trim();
        StringBuilder acronym = new StringBuilder();
        boolean isInside = false;

        for (byte b : clean.getBytes()) {
            if (b >= 'a' && b <= 'z') {
                if (!isInside) {
                    acronym.append((char) b);
                    isInside = true;
                }
            } else if (' ' == b) {
                isInside = false;
            }
        }

        return acronym.toString();
    }

    /**
     * Check if Lucene index needs to be rebuilt (e.g., due to format incompatibility).
     */
    public boolean luceneNeedsRebuild() {
        return luceneNeedsRebuild;
    }

    /**
     * Rebuild Lucene index from SQLite data.
     * Used when Lucene index format is incompatible.
     */
    public void rebuildLuceneFromSqlite() throws SQLException, IOException {
        logger.info("Rebuilding Lucene index from SQLite data...");
        long start = System.currentTimeMillis();

        try (Statement stmt = sqliteConnection.createStatement();
             ResultSet rs = stmt.executeQuery("SELECT id, title FROM atoms WHERE title IS NOT NULL")) {

            int count = 0;
            while (rs.next()) {
                String id = rs.getString("id");
                String title = rs.getString("title");

                Document doc = new Document();
                doc.add(new StringField(FIELD_ID, id, Field.Store.YES));
                doc.add(new TextField(FIELD_TITLE, title, Field.Store.YES));

                String acronym = generateAcronym(title);
                if (acronym != null && !acronym.isEmpty()) {
                    doc.add(new TextField(FIELD_ACRONYM, acronym, Field.Store.NO));
                }

                luceneWriter.updateDocument(new Term(FIELD_ID, id), doc);
                count++;

                if (count % 10000 == 0) {
                    logger.info("Rebuilt Lucene index for " + count + " atoms...");
                }
            }

            luceneWriter.commit();
            refreshLuceneReader();

            long elapsed = System.currentTimeMillis() - start;
            logger.info("Rebuilt Lucene index for " + count + " atoms in " + elapsed + "ms");
        }

        luceneNeedsRebuild = false;
    }

    @Override
    public void close() throws IOException {
        if (luceneReader != null) {
            luceneReader.close();
        }
        if (luceneWriter != null) {
            luceneWriter.close();
        }
        if (luceneDirectory != null) {
            luceneDirectory.close();
        }
        try {
            if (sqliteConnection != null && !sqliteConnection.isClosed()) {
                sqliteConnection.close();
            }
        } catch (SQLException e) {
            throw new IOException("Failed to close SQLite connection", e);
        }
    }
}
