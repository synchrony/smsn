package net.fortytwo.smsn.server.io;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomGraph;
import net.fortytwo.smsn.brain.Filter;
import net.fortytwo.smsn.brain.MyOtherBrain;
import net.fortytwo.smsn.brain.Note;
import net.fortytwo.smsn.brain.NoteQueries;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class FreeplaneImporter extends Importer {
    public static final String FORMAT = "Freeplane";

    private static final String
            ELEMENTNAME_ARROWLINK = "arrowlink",
            ELEMENTNAME_BODY = "body",
            ELEMENTNAME_MAP = "map",
            ELEMENTNAME_NODE = "node",
            ELEMENTNAME_RICHCONTENT = "richcontent";

    private static final String
            ATTR_CREATED = "CREATED",
            ATTR_DESTINATION = "DESTINATION",
            ATTR_ID = "ID",
            ATTR_MODIFIED = "MODIFIED",
            ATTR_LOCALIZED_STYLE_REF = "LOCALIZED_STYLE_REF",
            ATTR_TEXT = "TEXT";

    private static final String SCHEMA_PATH = "freeplane.xsd";

    private static final boolean USE_VALIDATION = true;

    @Override
    public List<String> getFormats() {
        return Arrays.asList(FORMAT);
    }

    private Map<AtomGraph, ParserInstance> parserInstancesByGraph = new HashMap<>();

    @Override
    protected void importInternal(MyOtherBrain destBrain, InputStream sourceStream) throws IOException {
        Document doc;
        try {
            doc = parseStreamToDocument(sourceStream);
        } catch (ParserConfigurationException | SAXException e) {
            throw new IOException(e);
        }

        getParserInstanceFor(destBrain.getAtomGraph()).parseDOMToGraph(doc);
    }

    private ParserInstance getParserInstanceFor(final AtomGraph destGraph) {
        ParserInstance instance = parserInstancesByGraph.get(destGraph);
        if (null == instance) {
            instance = new ParserInstance(destGraph);
            parserInstancesByGraph.put(destGraph, instance);
        }

        return instance;
    }

    private Document parseStreamToDocument(final InputStream sourceStream)
            throws ParserConfigurationException, IOException, org.xml.sax.SAXException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        if (USE_VALIDATION) configureValidation(factory);

        factory.setIgnoringElementContentWhitespace(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        return builder.parse(sourceStream);
    }

    // as of July 2016: http://www.freeplane.org/wiki/index.php/Current_Freeplane_File_Format
    private void configureValidation(DocumentBuilderFactory docFactory) throws SAXException {
        docFactory.setNamespaceAware(true);
        SchemaFactory schemaFactory =
                SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        docFactory.setValidating(true);
        Schema schema = schemaFactory.newSchema(new StreamSource(
                getClass().getResourceAsStream(SCHEMA_PATH)));
        docFactory.setSchema(schema);
    }

    private void persistNote(final AtomGraph destGraph, final Note rootNote)
            throws MyOtherBrain.BrainException, NoteQueries.InvalidUpdateException {

        int maxHeight = 1000;

        MyOtherBrain brain = new MyOtherBrain(destGraph);
        NoteQueries queries = new NoteQueries(brain);
        Filter filter = new Filter();

        Atom atom = destGraph.createAtom(filter, SemanticSynchrony.createRandomKey());
        rootNote.setId((String) atom.asVertex().getId());
        queries.update(rootNote, maxHeight, filter, NoteQueries.forwardViewStyle);
    }

    private void setText(Note note, String text) {
        System.out.println("text: " + text);
        note.setValue(text);
    }

    private String getRichContent(final Element element) {
        Element content = getSingleElement(element, ELEMENTNAME_RICHCONTENT);
        if (null == content) {
            return null;
        }

        Element body = getSingleElement(content, ELEMENTNAME_BODY);
        return body == null
                ? null
                : body.getTextContent();
    }

    private Element getSingleElement(final Element parent, final String name) {
        List<Element> children = getChildElements(parent, name);

        int length = children.size();
        switch (children.size()) {
            case 0:
                return null;
            case 1:
                return children.get(0);
            default:
                throw new IllegalArgumentException("expected exactly one <" + name
                        + "> node, found " + length);
        }
    }

    private List<Element> getChildElements(final Element element, final String name) {
        return getChildElements(element).stream().filter(el -> el.getNodeName().equals(name))
                .collect(Collectors.toList());
    }

    private List<Element> getChildElements(final Element element) {
        List<Element> asList = new LinkedList<>();
        NodeList children = element.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node node = children.item(i);
            if (node instanceof Element)
                asList.add((Element) node);
        }
        return asList;
    }

    private long getCreated(final Element element) {
        return getTimestamp(element, ATTR_CREATED);
    }

    private long getModified(final Element element) {
        return getTimestamp(element, ATTR_MODIFIED);
    }

    private String getStyle(final Element element) {
        return element.getAttribute(ATTR_LOCALIZED_STYLE_REF);
    }

    private long getTimestamp(final Element element, final String attrName) {
        String value = element.getAttribute(attrName);
        return (null == value || 0 == value.length())
                ? System.currentTimeMillis()
                : Long.valueOf(value);
    }

    private class ParserInstance {
        private final AtomGraph destGraph;
        private Map<String, List<String>> arrowLinks = new HashMap<>();
        private Map<String, Note> notesByFreeplaneId = new HashMap<>();
        private Map<String, Note> styleNotes = new HashMap<>();

        public ParserInstance(AtomGraph destGraph) {
            this.destGraph = destGraph;
        }

        private void parseDOMToGraph(Document document) throws IOException {
            Element root = document.getDocumentElement();

            if (!root.getTagName().equals(ELEMENTNAME_MAP)) {
                throw new IllegalArgumentException("root of mind map XML must be called 'map'");
            }

            Note mindMapAsNote = parseTree(root);
            try {
                persistNote(destGraph, mindMapAsNote);
            } catch (MyOtherBrain.BrainException | NoteQueries.InvalidUpdateException e) {
                throw new IOException(e);
            }

            persistArrowLinks();
        }

        private Atom getAtom(final String id) {
            return destGraph.getAtom(notesByFreeplaneId.get(id).getId());
        }

        private void persistArrowLinks() {
            for (Map.Entry<String, List<String>> link : arrowLinks.entrySet()) {
                Atom tailAtom = getAtom(link.getKey());
                List<String> heads = link.getValue();
                for (String head : heads) {
                    Atom headAtom = getAtom(head);
                    destGraph.addChildAt(tailAtom, headAtom, 0);
                }
            }
        }

        private Note parseTree(Element nodeElement) {
            Note note = new Note();
            note.setId(SemanticSynchrony.createRandomKey());

            String id = nodeElement.getAttribute(ATTR_ID);
            long created = getCreated(nodeElement);
            long modified = getModified(nodeElement);

            Note styleNote = getStyleNote(getStyle(nodeElement));

            notesByFreeplaneId.put(id, note);
            note.setCreated(created);
            // TODO: make id and modified date into property values

            String text = nodeElement.getAttribute(ATTR_TEXT);
            if (null == text || 0 == text.length()) {
                text = getRichContent(nodeElement);
                if (null == text) {
                    text = getDefaultNodeName();
                }
            }
            setText(note, text);

            if (null != styleNote) {
                note.setHasChildren(true);
                note.addChild(styleNote);
            }

            parseChildren(note, nodeElement, id);

            return note;
        }

        private void parseChildren(final Note note, final Element nodeElement, String nodeId) {
            List<Element> backlinkChildren = getChildElements(nodeElement, ELEMENTNAME_ARROWLINK);
            if (0 != backlinkChildren.size()) {
                List<String> heads = arrowLinks.get(nodeId);
                if (null == heads) {
                    heads = new LinkedList<>();
                    arrowLinks.put(nodeId, heads);
                }
                for (Element head : backlinkChildren) {
                    String headId = head.getAttribute(ATTR_DESTINATION);
                    if (null != headId) {
                        note.setHasChildren(true);
                        heads.add(headId);
                    }
                }
            }

            // note: non-content <hook> elements are ignored
            List<Element> treeChildren = getChildElements(nodeElement, ELEMENTNAME_NODE);
            if (0 != treeChildren.size()) {
                note.setHasChildren(true);
                for (Element childElement : treeChildren) {
                    Note child = parseTree(childElement);
                    note.addChild(child);
                }
            }
        }

        private Note getStyleNote(final String style) {
            if (null == style || style.length() == 0) return null;

            Note note = styleNotes.get(style);
            if (null == note) {
                note = new Note();
                note.setValue(style + " (style)");
                note.setId(SemanticSynchrony.createRandomKey());
                styleNotes.put(style, note);
            }
            return note;
        }
    }
}
