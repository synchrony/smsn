package net.fortytwo.smsn.brain.io.yaml;

import org.yaml.snakeyaml.nodes.NodeId;
import org.yaml.snakeyaml.nodes.Tag;
import org.yaml.snakeyaml.resolver.Resolver;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class CustomResolver extends Resolver {

    protected Map<Character, List<ResolverTuple>> yamlImplicitResolvers;

    @Override
    protected void addImplicitResolvers() {
    }

    public CustomResolver() {
        yamlImplicitResolvers = new HashMap<>();
        addImplicitResolvers();
    }

    @Override
    public void addImplicitResolver(Tag tag, Pattern regexp, String first) {
    }

    @Override
    public Tag resolve(NodeId kind, String value, boolean implicit) {
        if (kind == NodeId.scalar && implicit) {
            List<ResolverTuple> resolvers = null;
            if (value.length() == 0) {
                resolvers = yamlImplicitResolvers.get('\0');
            } else {
                resolvers = yamlImplicitResolvers.get(value.charAt(0));
            }
            if (resolvers != null) {
                for (ResolverTuple v : resolvers) {
                    Tag tag = v.getTag();
                    Pattern regexp = v.getRegexp();
                    if (regexp.matcher(value).matches()) {
                        return tag;
                    }
                }
            }
            if (yamlImplicitResolvers.containsKey(null)) {
                for (ResolverTuple v : yamlImplicitResolvers.get(null)) {
                    Tag tag = v.getTag();
                    Pattern regexp = v.getRegexp();
                    if (regexp.matcher(value).matches()) {
                        return tag;
                    }
                }
            }
        }
        switch (kind) {
            case scalar:
                return Tag.STR;
            case sequence:
                return Tag.SEQ;
            default:
                return Tag.MAP;
        }
    }

    private final class ResolverTuple {
        private final Tag tag;
        private final Pattern regexp;

        public ResolverTuple(Tag tag, Pattern regexp) {
            this.tag = tag;
            this.regexp = regexp;
        }

        public Tag getTag() {
            return tag;
        }

        public Pattern getRegexp() {
            return regexp;
        }

        @Override
        public String toString() {
            return "Tuple tag=" + tag + " regexp=" + regexp;
        }
    }
}
