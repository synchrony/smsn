package net.fortytwo.smsn.server;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.TreeViews;
import net.fortytwo.smsn.brain.io.json.JsonParser;
import net.fortytwo.smsn.brain.io.json.JsonPrinter;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;

import java.util.Map;

public class RequestParams {
    private GraphWrapper graphWrapper;
    private Brain brain;
    private String data;
    private Integer height;
    private String file;
    private Filter filter;
    private String format;
    private boolean includeTypes;
    private Map<String, Object> map;
    private Integer maxResults;
    private String propertyName;
    private Object propertyValue;
    private TreeViews queries;
    private String query;
    private TreeViews.QueryType queryType;
    private Atom root;
    private String rootId;
    private TreeViews.ViewStyle style;
    private String styleName;
    private Integer titleCutoff;
    private String view;
    private WikiParser wikiParser;
    private JsonParser jsonParser;
    private JsonPrinter jsonPrinter;

    public GraphWrapper getGraphWrapper() {
        return graphWrapper;
    }

    public void setGraphWrapper(GraphWrapper graphWrapper) {
        this.graphWrapper = graphWrapper;
    }

    public Brain getBrain() {
        return brain;
    }

    public void setBrain(Brain brain) {
        this.brain = brain;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public Integer getHeight() {
        return height;
    }

    public void setHeight(Integer height) {
        this.height = height;
    }

    public String getFile() {
        return file;
    }

    public void setFile(String file) {
        this.file = file;
    }

    public Filter getFilter() {
        return filter;
    }

    public void setFilter(Filter filter) {
        this.filter = filter;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public boolean isIncludeTypes() {
        return includeTypes;
    }

    public void setIncludeTypes(boolean includeTypes) {
        this.includeTypes = includeTypes;
    }

    public Map<String, Object> getMap() {
        return map;
    }

    public void setMap(Map<String, Object> map) {
        this.map = map;
    }

    public Integer getMaxResults() {
        return maxResults;
    }

    public void setMaxResults(Integer maxResults) {
        this.maxResults = maxResults;
    }

    public String getPropertyName() {
        return propertyName;
    }

    public void setPropertyName(String propertyName) {
        this.propertyName = propertyName;
    }

    public Object getPropertyValue() {
        return propertyValue;
    }

    public void setPropertyValue(Object propertyValue) {
        this.propertyValue = propertyValue;
    }

    public TreeViews getQueries() {
        return queries;
    }

    public void setQueries(TreeViews queries) {
        this.queries = queries;
    }

    public String getQuery() {
        return query;
    }

    public void setQuery(String query) {
        this.query = query;
    }

    public TreeViews.QueryType getQueryType() {
        return queryType;
    }

    public void setQueryType(TreeViews.QueryType queryType) {
        this.queryType = queryType;
    }

    public Atom getRoot() {
        return root;
    }

    public void setRoot(Atom root) {
        this.root = root;
    }

    public String getRootId() {
        return rootId;
    }

    public void setRootId(String rootId) {
        this.rootId = rootId;
    }

    public TreeViews.ViewStyle getStyle() {
        return style;
    }

    public void setStyle(TreeViews.ViewStyle style) {
        this.style = style;
    }

    public String getStyleName() {
        return styleName;
    }

    public void setStyleName(String styleName) {
        this.styleName = styleName;
    }

    public Integer getTitleCutoff() {
        return titleCutoff;
    }

    public void setTitleCutoff(Integer titleCutoff) {
        this.titleCutoff = titleCutoff;
    }

    public String getView() {
        return view;
    }

    public void setView(String wikiView) {
        this.view = wikiView;
    }

    public JsonPrinter getJsonPrinter() {
        return jsonPrinter;
    }

    public void setJsonPrinter(JsonPrinter jsonPrinter) {
        this.jsonPrinter = jsonPrinter;
    }

    public WikiParser getWikiParser() {
        return wikiParser;
    }

    public void setWikiParser(WikiParser wikiParser) {
        this.wikiParser = wikiParser;
    }

    public JsonParser getJsonParser() {
        return jsonParser;
    }

    public void setJsonParser(JsonParser jsonParser) {
        this.jsonParser = jsonParser;
    }
}
