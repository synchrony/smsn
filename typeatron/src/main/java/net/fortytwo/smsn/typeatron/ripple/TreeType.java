package net.fortytwo.smsn.typeatron.ripple;

import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.io.RipplePrintStream;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.StackMapping;
import net.fortytwo.ripple.model.types.SimpleType;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;

public class TreeType extends SimpleType<TreeNode<Link>> {

    private static final ValueFactory valueFactory = ValueFactoryImpl.getInstance();

    public TreeType() {
        super(TreeNode.class);
    }

    @Override
    public boolean isInstance(TreeNode<Link> instance) {
        return true;
    }

    @Override
    public Value toRDF(TreeNode<Link> instance, ModelConnection mc) throws RippleException {
        // if the note has an alias, use that as its IRI.
        // Otherwise, use the "thing" IRI based on its ID
        // This has the effect of mapping to an externally-defined resource if the given note has an alias,
        // otherwise mapping to a reference to a thing described in the personal knowledge base.
        // In the latter case, the resource will only be accessible in an interactive setting if the thing is a public,
        // classified note and the knowledge base has been appropriately published as Linked Data.
        return null == TreeViews.getAlias(instance)
                ? null == TreeViews.getId(instance) ? null : valueFactory.createIRI(
                        iriForId(TreeViews.getId(instance)))
                : valueFactory.createIRI(TreeViews.getAlias(instance));
    }

    @Override
    public StackMapping getMapping(TreeNode<Link> instance) {
        return null;
    }

    @Override
    public void print(TreeNode<Link> instance, RipplePrintStream p, ModelConnection mc) throws RippleException {
        p.print(instance);
    }

    @Override
    public Category getCategory() {
        return Category.OTHER_RESOURCE;
    }

    @Override
    public int compare(TreeNode<Link> o1, TreeNode<Link> o2, ModelConnection mc) {
        // compare by unique id
        // note: an alternative is comparison by creation time
        return null == TreeViews.getId(o1)
                ? null == TreeViews.getId(o2) ? 0 : -1
                : null == TreeViews.getId(o2) ? 1 : TreeViews.getId(o1).value.compareTo(TreeViews.getId(o2).value);
    }

    private static String iriForId(AtomId id) {
        return "urn:smsn:" + id.value;
    }
}
