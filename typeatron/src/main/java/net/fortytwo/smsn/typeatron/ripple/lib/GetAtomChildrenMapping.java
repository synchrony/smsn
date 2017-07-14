package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.List;
import java.util.logging.Logger;

public class GetAtomChildrenMapping extends AtomMapping {

    private static final Logger logger = Logger.getLogger(GetAtomChildrenMapping.class.getName());

    public GetAtomChildrenMapping(final BrainClient client,
                                  final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "get-atom-children"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("atom", "the reference atom", true)};
    }

    public String getComment() {
        return "gets the list of children of an atom";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        stack = stack.getRest();

        TreeNode<Link> n = toTree(first, 1, true);

        if (null == n) {
            logger.warning("can't get children of non-atom: " + first);
        } else {
            List<TreeNode<Link>> children = TreeViews.getChildrenAsList(n);
            if (null != children && 0 != children.size()) {
                RippleList cur = mc.list();
                for (int i = children.size() - 1; i >= 0; i--) {
                    cur = cur.push(children.get(i));
                }
                // put both the children and the (synced) atom back on the stack
                solutions.accept(stack.push(n).push(cur));
            }
        }
    }
}
