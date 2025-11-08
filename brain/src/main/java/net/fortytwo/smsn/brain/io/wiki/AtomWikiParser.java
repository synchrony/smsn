package net.fortytwo.smsn.brain.io.wiki;

import hydra.util.Opt;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.Timestamp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses .smsn files in Wiki format to create Atom objects.
 * This replaces the old WikiParser which created Page/TreeNode<Link>.
 */
public class AtomWikiParser {

    private static final Pattern PROPERTY_PATTERN = Pattern.compile("^@(\\w+)\\s+(.*)$");
    private static final Pattern CHILD_PATTERN = Pattern.compile("^\\*\\s+:([^:]+):");

    public Atom parse(final InputStream inputStream) throws IOException {
        return parseInternal(inputStream);
    }

    private Atom parseInternal(final InputStream inputStream) throws IOException {
        Map<String, String> properties = new HashMap<>();
        List<AtomId> children = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream, SemanticSynchrony.UTF8))) {
            String line;
            String multilineKey = null;
            StringBuilder multilineValue = new StringBuilder();

            while ((line = br.readLine()) != null) {
                // Handle multiline values enclosed in ```
                if (multilineKey != null) {
                    if (line.trim().equals("```")) {
                        // End of multiline value
                        properties.put(multilineKey, multilineValue.toString());
                        multilineKey = null;
                        multilineValue = new StringBuilder();
                    } else {
                        if (multilineValue.length() > 0) {
                            multilineValue.append("\n");
                        }
                        multilineValue.append(line);
                    }
                    continue;
                }

                // Parse property line: @key value
                Matcher propertyMatcher = PROPERTY_PATTERN.matcher(line);
                if (propertyMatcher.matches()) {
                    String key = propertyMatcher.group(1);
                    String value = propertyMatcher.group(2);

                    if (value.equals("```")) {
                        // Start of multiline value
                        multilineKey = key;
                    } else {
                        properties.put(key, value);
                    }
                    continue;
                }

                // Parse child reference: "* :childId:"
                Matcher childMatcher = CHILD_PATTERN.matcher(line);
                if (childMatcher.find()) {
                    String childId = childMatcher.group(1);
                    children.add(new AtomId(childId));
                }
            }
        }

        return buildAtom(properties, children);
    }

    private Atom buildAtom(Map<String, String> properties, List<AtomId> children) {
        // Required fields with defaults
        AtomId id = properties.containsKey("id")
                ? new AtomId(properties.get("id"))
                : new AtomId("temp-" + System.nanoTime());

        // created is stored as milliseconds in .smsn files
        Timestamp created = properties.containsKey("created")
                ? new Timestamp(Long.parseLong(properties.get("created")))
                : new Timestamp(System.currentTimeMillis());

        Normed weight = properties.containsKey("weight")
                ? new Normed(Float.parseFloat(properties.get("weight")))
                : new Normed(0.5f);

        SourceName source = properties.containsKey("source")
                ? new SourceName(properties.get("source"))
                : new SourceName("public");

        String title = properties.getOrDefault("title", "");

        // Optional fields
        Opt<Normed> priority = properties.containsKey("priority")
                ? Opt.of(new Normed(Float.parseFloat(properties.get("priority"))))
                : Opt.empty();

        Opt<String> alias = properties.containsKey("alias")
                ? Opt.of(properties.get("alias"))
                : Opt.empty();

        Opt<String> text = properties.containsKey("text")
                ? Opt.of(properties.get("text"))
                : Opt.empty();

        Opt<String> shortcut = properties.containsKey("shortcut")
                ? Opt.of(properties.get("shortcut"))
                : Opt.empty();

        return new Atom(id, created, weight, priority, source, title, alias, text, shortcut, children);
    }
}
