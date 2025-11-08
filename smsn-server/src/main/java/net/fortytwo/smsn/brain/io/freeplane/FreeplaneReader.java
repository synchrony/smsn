package net.fortytwo.smsn.brain.io.freeplane;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class FreeplaneReader extends NoteReader {

    private interface Elmt {
        String
                ARROWLINK = "arrowlink",
                BODY = "body",
                MAP = "map",
                NODE = "node",
                RICHCONTENT = "richcontent";
    }

    private interface Attr {
        String
                CREATED = "CREATED",
                DESTINATION = "DESTINATION",
                ID = "ID",
                MODIFIED = "MODIFIED",
                LOCALIZED_STYLE_REF = "LOCALIZED_STYLE_REF",
                TEXT = "TEXT";
    }

    //private static final String JAXP_SCHEMA_LANGUAGE = "http://java.sun.com/xml/jaxp/properties/schemaLanguage";
    //private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    private static final String SCHEMA_PATH = "freeplane.xsd";

    private static final boolean USE_VALIDATION = true;

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(FreeplaneFormat.getInstance());
    }

    private final Map<TopicGraph, ParserInstance> parserInstancesByGraph = new HashMap<>();

    private final FreeplaneXMLParser xmlParser;

    public FreeplaneReader() throws ParserConfigurationException, SAXException {
        xmlParser = new FreeplaneXMLParser();
    }

    @Override
    protected void importInternal(Context context)
            throws IOException {

        Document doc;
        try {
            doc = xmlParser.parseStreamToDocument(context.getSourceStream());
        } catch (SAXException e) {
            throw new IOException(e);
        }

        getParserInstanceFor(context.getTopicGraph()).parseDOMToGraph(doc);
    }

    private ParserInstance getParserInstanceFor(final TopicGraph destGraph) {
        ParserInstance instance = parserInstancesByGraph.get(destGraph);
        if (null == instance) {
            instance = new ParserInstance(destGraph);
            parserInstancesByGraph.put(destGraph, instance);
        }

        return instance;
    }

    private void persistNote(final TopicGraph destGraph, final TreeData rootNote)
            throws Brain.BrainException {

        Brain brain = new Brain(destGraph);
        AtomRepository repository = brain.getAtomRepository();
        Filter filter = Filter.noFilter();

        // Create atoms recursively from the tree structure
        persistTreeData(repository, rootNote, filter);

        checkAndCommit(destGraph);
    }

    /**
     * Recursively persist TreeData as Atoms in the repository.
     */
    private void persistTreeData(AtomRepository repository, TreeData treeData, Filter filter) {
        // Recursively persist children first
        java.util.List<AtomId> childIds = new java.util.ArrayList<>();
        for (TreeData child : treeData.getChildren()) {
            persistTreeData(repository, child, filter);
            childIds.add(child.getId());
        }

        // Create the atom with all properties including children
        Atom atom = TreeViewBuilder.createAtom(
                treeData.getId(),
                treeData.getCreated(),
                SemanticSynchrony.DEFAULT_WEIGHT,
                filter.getDefaultSource(),
                treeData.getTitle() != null ? treeData.getTitle() : "[no title]",
                treeData.getText(),
                childIds
        );

        // Save to repository
        repository.save(atom);
    }

    private void setTextOrTitle(TreeData note, String text) {
        if (text.contains("\n")) {
            note.setText(text);
            note.setTitle("[multiple lines]");
        } else {
            note.setTitle(text);
        }
    }

    private String trim(String text) {
        if (null == text) {
            return null;
        }
        text = text.trim();
        if (0 == text.length()) {
            return null;
        } else {
            return text;
        }
    }

    private String getText(final Element nodeElement) {
        String text = trim(nodeElement.getAttribute(Attr.TEXT));
        if (null != text) {
            return text;
        }

        text = trim(getRichContent(nodeElement));
        if (null != text) {
            return text;
        }

        return "[no text]";
    }

    private String getRichContent(final Element element) {
        Element content = getSingleElement(element, Elmt.RICHCONTENT);
        if (null == content) {
            return null;
        }

        return content.getTextContent();
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
        return getTimestamp(element, Attr.CREATED);
    }

    private long getModified(final Element element) {
        return getTimestamp(element, Attr.MODIFIED);
    }

    private String getStyle(final Element element) {
        return element.getAttribute(Attr.LOCALIZED_STYLE_REF);
    }

    private long getTimestamp(final Element element, final String attrName) {
        String value = element.getAttribute(attrName);
        return (null == value || 0 == value.length())
                ? System.currentTimeMillis()
                : Long.valueOf(value);
    }

    private class ParserInstance {
        private final TopicGraph destGraph;
        private final Map<String, List<String>> arrowLinks = new HashMap<>();
        private final Map<String, TreeData> notesByFreeplaneId = new HashMap<>();
        private final Map<String, TreeData> styleNotes = new HashMap<>();

        public ParserInstance(TopicGraph destGraph) {
            this.destGraph = destGraph;
        }

        private void parseDOMToGraph(Document document) throws IOException, InvalidGraphException {
            Element mapNode = document.getDocumentElement();
            if (!mapNode.getTagName().equals(Elmt.MAP)) {
                throw new IllegalArgumentException("root of mind map XML must be called 'map'");
            }
            NodeList children = mapNode.getChildNodes();
            Element rootNode = null;
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);
                if (child instanceof Element) {
                    if (null != rootNode) {
                        throw new IllegalArgumentException("multiple root nodes");
                    }
                    rootNode = (Element) child;
                }
            }
            if (null == rootNode) {
                throw new IllegalArgumentException("no root node");
            }

            resetArrowLinks();

            TreeData mindMapAsNote = parseTree(rootNode);
            try {
                persistNote(destGraph, mindMapAsNote);
            } catch (Brain.BrainException e) {
                throw new IOException(e);
            }

            persistArrowLinks();
        }

        private AtomId getAtomId(final String freeplaneId) {
            return notesByFreeplaneId.get(freeplaneId).getId();
        }

        private void persistArrowLinks() throws InvalidGraphException {
            for (Map.Entry<String, List<String>> link : arrowLinks.entrySet()) {
                AtomId tailId = getAtomId(link.getKey());
                List<String> heads = link.getValue();
                for (String head : heads) {
                    AtomId headId = getAtomId(head);
                    // Use TopicGraph to add the child relationship
                    var tailNote = destGraph.getNoteById(tailId).orElseThrow();
                    var headNote = destGraph.getNoteById(headId).orElseThrow();
                    tailNote.addChildAt(headNote, 0);
                }
            }
        }

        private void resetArrowLinks() {
            arrowLinks.clear();
        }

        private TreeData parseTree(Element nodeElement) {
            TreeData root = new TreeData();
            root.setId(SemanticSynchrony.createRandomId());

            String id = nodeElement.getAttribute(Attr.ID);
            long created = getCreated(nodeElement);
            long modified = getModified(nodeElement);

            TreeData styleNote = getStyleNote(getStyle(nodeElement));

            notesByFreeplaneId.put(id, root);
            root.setCreated(created);
            // TODO: make id and modified date into property values

            setTextOrTitle(root, getText(nodeElement));

            if (null != styleNote) {
                root.addChild(styleNote);
            }

            parseChildren(root, nodeElement, id);

            makeLegal(root);

            return root;
        }

        private void makeLegal(final TreeData note) {
            if (null != note.getText() && note.countChildren() > 0) {
                note.setTitle(note.getText().replaceAll("\\n", "\\n"));
                logger.warning("note had both children and multi-line text (collapsed): "
                        + titlePreview(note.getTitle()));
            }
        }

        private String titlePreview(final String title) {
            return title.length() > 30 ? title.substring(0, 25) + "[...]" : title;
        }

        private void parseChildren(final TreeData note, final Element nodeElement, String nodeId) {
            List<Element> backlinkChildren = getChildElements(nodeElement, Elmt.ARROWLINK);
            if (0 != backlinkChildren.size()) {
                List<String> heads = arrowLinks.get(nodeId);
                if (null == heads) {
                    heads = new LinkedList<>();
                    arrowLinks.put(nodeId, heads);
                }
                for (Element head : backlinkChildren) {
                    String headId = head.getAttribute(Attr.DESTINATION);
                    if (null != headId) {
                        //note.setHasChildren(true);
                        heads.add(headId);
                    }
                }
            }

            // note: non-content <hook> elements are ignored
            List<Element> treeChildren = getChildElements(nodeElement, Elmt.NODE);
            if (0 != treeChildren.size()) {
                //note.setHasChildren(true);
                for (Element childElement : treeChildren) {
                    TreeData child = parseTree(childElement);
                    note.addChild(child);
                }
            }
        }

        private TreeData getStyleNote(final String style) {
            if (null == style || style.length() == 0) return null;

            TreeData note = styleNotes.get(style);
            if (null == note) {
                note = new TreeData();
                note.setTitle(style + " (style)");
                note.setId(SemanticSynchrony.createRandomId());
                note.setCreated(System.currentTimeMillis());
                styleNotes.put(style, note);
            }
            return note;
        }
    }

    private static class FreeplaneXMLParser extends DefaultHandler {
        private final DocumentBuilder builder;

        public FreeplaneXMLParser() throws SAXException, ParserConfigurationException {
            DocumentBuilderFactory factory = createDocumentBuilderFactory();
            builder = factory.newDocumentBuilder();

            // note: this suppresses error messages relating to the not-quite-accurate Freeplane schema
            builder.setErrorHandler(new DefaultHandler());
        }

        private Document parseStreamToDocument(final InputStream sourceStream)
                throws IOException, org.xml.sax.SAXException {
            return builder.parse(sourceStream);
        }

        private DocumentBuilderFactory createDocumentBuilderFactory() throws SAXException {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setIgnoringElementContentWhitespace(true);

            if (USE_VALIDATION) configureValidation(factory);

            return factory;
        }

        // as of July 2016: http://www.freeplane.org/wiki/index.php/Current_Freeplane_File_Format
        private void configureValidation(DocumentBuilderFactory docFactory) throws SAXException {
            docFactory.setNamespaceAware(false);
            //docFactory.setAttribute(JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);
            SchemaFactory schemaFactory =
                    SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = schemaFactory.newSchema(new StreamSource(
                    getClass().getResourceAsStream(SCHEMA_PATH)));
            docFactory.setSchema(schema);
            docFactory.setValidating(true);
        }
    }
}
