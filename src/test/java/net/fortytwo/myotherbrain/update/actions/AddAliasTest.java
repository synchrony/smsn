package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.model.concepts.Atom;
import net.fortytwo.myotherbrain.model.concepts.Literal;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import org.openrdf.concepts.owl.Thing;

/**
 * Author: josh
 * Date: Jul 3, 2009
 * Time: 12:00:01 PM
 */
public class AddAliasTest extends WriteActionTestCase {

    public void testAll() throws Exception {
        WriteContext c = new WriteContext(model.createConnection());

        Atom something = c.create(Atom.class);
        Thing alias1 = c.create(Thing.class);
        Literal alias2 = c.create(Literal.class);

        WriteAction action1 = new AddAlias(MyOtherBrain.toURI(something.getQName()),
                MyOtherBrain.toURI(alias1.getQName()), c);
        WriteAction action2 = new AddAlias(MyOtherBrain.toURI(something.getQName()),
                MyOtherBrain.toURI(alias2.getQName()), c);

        assertEquals(0, something.getAlias().size());

        action1.redo(c);
        assertEquals(1, something.getAlias().size());
        assertTrue(something.getAlias().contains(alias1));

        action1.undo(c);
        assertEquals(0, something.getAlias().size());

        action1.redo(c);
        action2.redo(c);
        assertEquals(2, something.getAlias().size());
        assertTrue(something.getAlias().contains(alias1));
        assertTrue(something.getAlias().contains(alias2));

        action2.undo(c);
        assertEquals(1, something.getAlias().size());
        assertTrue(something.getAlias().contains(alias1));
        
        action1.undo(c);
        assertEquals(0, something.getAlias().size());

        c.getConnection().rollback();
        c.getConnection().close();
    }
}
