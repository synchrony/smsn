package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.wiki.NoteParser;
import net.fortytwo.smsn.brain.wiki.NoteWriter;
import org.junit.Before;

import java.io.IOException;

public abstract class BrainTestBase {
    protected final NoteParser parser = new NoteParser();
    protected final NoteWriter writer = new NoteWriter();

    protected NoteQueries queries;
    protected Filter filter;

    protected AtomGraph atomGraph;
    protected abstract AtomGraph createAtomGraph() throws IOException;

    @Before
    public void setUp() throws Exception {
        atomGraph = createAtomGraph();
        Brain brain = new Brain(atomGraph);
        queries = new NoteQueries(brain);
        filter = new Filter();
    }

    protected Atom importExample(final String exampleFile) throws IOException, NoteParser.NoteParsingException {
        Filter writeFilter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;

        Note rootNote = parser.fromWikiText(NoteParser.class.getResourceAsStream(exampleFile));
        rootNote.setId(SemanticSynchrony.createRandomKey());
        Atom root = atomGraph.createAtom(filter, rootNote.getId());
        queries.update(rootNote, 5, writeFilter, style);
        return root;
    }
}
