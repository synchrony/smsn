package net.fortytwo.smsn.brain.io.freeplane;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Tag;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
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

    private void persistNote(final TopicGraph destGraph, final TreeNode<Link> rootNote)
            throws Brain.BrainException {

        int maxHeight = 1000;

        Brain brain = new Brain(destGraph);
        TreeViews queries = new TreeViews(brain);
        Filter filter = Filter.noFilter();

        Note note = destGraph.createNoteWithProperties(filter, SemanticSynchrony.createRandomId());
        TreeViews.setId(rootNote, note.getId());

        queries.update(rootNote, maxHeight, filter, ViewStyle.Basic.Forward.getStyle());

        checkAndCommit(destGraph);
    }

    private void setTextOrTitle(TreeNode<Link> note, String text) {
        if (text.contains("\n")) {
            TreeViews.setText(note, text);
            TreeViews.setTitle(note, "[multiple lines]");
        } else {
            TreeViews.setTitle(note, text);
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
        private final Map<String, TreeNode<Link>> notesByFreeplaneId = new HashMap<>();
        private final Map<String, TreeNode<Link>> styleNotes = new HashMap<>();

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

            TreeNode<Link> mindMapAsNote = parseTree(rootNode);
            try {
                persistNote(destGraph, mindMapAsNote);
            } catch (Brain.BrainException e) {
                throw new IOException(e);
            }

            persistArrowLinks();
        }

        private Note getAtom(final String id) {
            return destGraph.getNotesById(TreeViews.getId(notesByFreeplaneId.get(id))).get();
        }

        private void persistArrowLinks() throws InvalidGraphException {
            for (Map.Entry<String, List<String>> link : arrowLinks.entrySet()) {
                Note tailAtom = getAtom(link.getKey());
                List<String> heads = link.getValue();
                for (String head : heads) {
                    Note headAtom = getAtom(head);
                    tailAtom.addChildAt(headAtom, 0);
                }
            }
        }

        private void resetArrowLinks() {
            arrowLinks.clear();
        }

        private TreeNode<Link> parseTree(Element nodeElement) {
            TreeNode<Link> root = TreeNodeDTO.createEmptyNode();
            TreeViews.setId(root, SemanticSynchrony.createRandomId());

            String id = nodeElement.getAttribute(Attr.ID);
            long created = getCreated(nodeElement);
            long modified = getModified(nodeElement);

            TreeNode<Link> styleNote = getStyleNote(getStyle(nodeElement));

            notesByFreeplaneId.put(id, root);
            TreeViews.setCreated(root, created);
            // TODO: make id and modified date into property values

            setTextOrTitle(root, getText(nodeElement));

            if (null != styleNote) {
                root.addChild(styleNote);
            }

            parseChildren(root, nodeElement, id);

            makeLegal(root);

            return root;
        }

        private void makeLegal(final TreeNode<Link> note) {
            if (null != TreeViews.getText(note) && TreeViews.countChildren(note) > 0) {
                TreeViews.setTitle(note, TreeViews.getText(note).replaceAll("\\n", "\\n"));
                logger.warning("note had both children and multi-line text (collapsed): "
                        + titlePreview(TreeViews.getTitle(note)));
            }
        }

        private String titlePreview(final String title) {
            return title.length() > 30 ? title.substring(0, 25) + "[...]" : title;
        }

        private void parseChildren(final TreeNode<Link> note, final Element nodeElement, String nodeId) {
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
                    TreeNode<Link> child = parseTree(childElement);
                    note.addChild(child);
                }
            }
        }

        private TreeNode<Link> getStyleNote(final String style) {
            if (null == style || style.length() == 0) return null;

            TreeNode<Link> note = styleNotes.get(style);
            if (null == note) {
                note = TreeNodeDTO.createEmptyNode();
                TreeViews.setTitle(note, style + " (style)");
                TreeViews.setId(note, SemanticSynchrony.createRandomId());
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
