package net.fortytwo.smsn.brain.io.freeplane;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class FreeplaneReader extends BrainReader {

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
        return Arrays.asList(FreeplaneFormat.getInstance());
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

    private void persistNote(final TopicGraph destGraph, final Note rootNote)
            throws Brain.BrainException {

        int maxHeight = 1000;

        Brain brain = new Brain(destGraph);
        TreeViews queries = new TreeViews(brain);
        Filter filter = Filter.noFilter();

        Atom atom = destGraph.createAtomWithProperties(filter, SemanticSynchrony.createRandomId());
        rootNote.setId(atom.getId());
        queries.update(rootNote, maxHeight, filter, ViewStyle.Basic.Forward.getStyle());

        checkAndCommit(destGraph);
    }

    private void setTextOrTitle(Note note, String text) {
        if (text.contains("\n")) {
            note.setPage(text);
            note.setTitle("RenameMe");
        }
        note.setTitle(text);
    }

    private String getRichContent(final Element element) {
        Element content = getSingleElement(element, Elmt.RICHCONTENT);
        if (null == content) {
            return null;
        }

        Element body = getSingleElement(content, Elmt.BODY);
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
        private final Map<String, Note> notesByFreeplaneId = new HashMap<>();
        private final Map<String, Note> styleNotes = new HashMap<>();

        public ParserInstance(TopicGraph destGraph) {
            this.destGraph = destGraph;
        }

        private void parseDOMToGraph(Document document) throws IOException, InvalidGraphException {
            Element root = document.getDocumentElement();

            if (!root.getTagName().equals(Elmt.MAP)) {
                throw new IllegalArgumentException("root of mind map XML must be called 'map'");
            }

            resetArrowLinks();

            Note mindMapAsNote = parseTree(root);
            try {
                persistNote(destGraph, mindMapAsNote);
            } catch (Brain.BrainException e) {
                throw new IOException(e);
            }

            persistArrowLinks();
        }

        private Atom getAtom(final String id) {
            return destGraph.getAtomById(notesByFreeplaneId.get(id).getId());
        }

        private void persistArrowLinks() throws InvalidGraphException {
            for (Map.Entry<String, List<String>> link : arrowLinks.entrySet()) {
                Atom tailAtom = getAtom(link.getKey());
                List<String> heads = link.getValue();
                for (String head : heads) {
                    Atom headAtom = getAtom(head);
                    tailAtom.addChildAt(headAtom, 0);
                }
            }
        }

        private void resetArrowLinks() {
            arrowLinks.clear();
        }

        private Note parseTree(Element nodeElement) {
            Note note = new Note();
            note.setId(SemanticSynchrony.createRandomId());

            String id = nodeElement.getAttribute(Attr.ID);
            long created = getCreated(nodeElement);
            long modified = getModified(nodeElement);

            Note styleNote = getStyleNote(getStyle(nodeElement));

            notesByFreeplaneId.put(id, note);
            note.setCreated(created);
            // TODO: make id and modified date into property values

            String text = nodeElement.getAttribute(Attr.TEXT);
            if (null == text || 0 == text.length()) {
                text = getRichContent(nodeElement);
                if (null == text) {
                    text = getDefaultNodeName();
                }
            }
            setTextOrTitle(note, text);

            if (null != styleNote) {
                //note.setHasChildren(true);
                note.addChild(styleNote);
            }

            parseChildren(note, nodeElement, id);

            makeLegal(note);

            return note;
        }

        private void makeLegal(final Note note) {
            if (null != note.getPage() && note.getChildren().size() > 0) {
                note.setTitle(note.getPage().replaceAll("\\n", "\\n"));
                logger.warning("note had both children and multi-line text (collapsed): "
                        + titlePreview(note.getTitle()));
            }
        }

        private String titlePreview(final String title) {
            return title.length() > 30 ? title.substring(0, 25) + "[...]" : title;
        }

        private void parseChildren(final Note note, final Element nodeElement, String nodeId) {
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
                note.setTitle(style + " (style)");
                note.setId(SemanticSynchrony.createRandomId());
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
