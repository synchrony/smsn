package net.fortytwo.smsn.brain.repository;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.pg.Sortable;
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
    private static final int MAX_SEARCH_RESULTS = 100;

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

    public IndexManager(File indexDirectory) throws IOException, SQLException {
        this.indexDirectory = indexDirectory;
        if (!indexDirectory.exists()) {
            indexDirectory.mkdirs();
        }

        // Initialize SQLite
        File sqliteFile = new File(indexDirectory, "index.db");
        String jdbcUrl = "jdbc:sqlite:" + sqliteFile.getAbsolutePath();
        this.sqliteConnection = DriverManager.getConnection(jdbcUrl);
        initializeSqliteTables();

        // Initialize Lucene
        Path lucenePath = new File(indexDirectory, "lucene").toPath();
        this.luceneDirectory = FSDirectory.open(lucenePath);
        this.analyzer = new StandardAnalyzer();

        IndexWriterConfig config = new IndexWriterConfig(analyzer);
        config.setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND);
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
        indexAtomInLucene(atom);
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

    private void indexAtomInLucene(Atom atom) throws IOException {
        // Delete existing document
        luceneWriter.deleteDocuments(new Term(FIELD_ID, atom.id.value));

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

        try {
            QueryParser parser = new QueryParser(field, analyzer);
            parser.setDefaultOperator(QueryParser.Operator.AND);
            Query query = parser.parse(queryString);

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
