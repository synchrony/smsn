package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.Literal;
import org.openrdf.concepts.owl.Thing;

import java.util.HashSet;
import java.util.Set;

/**
 * Author: josh
 * Date: Jul 3, 2009
 * Time: 12:00:01 PM
 */
public class RemoveAliasTest extends WriteActionTestCase {

    public void testAll() throws Exception {
        MOBModelConnection c = model.createConnection();

        FirstClassItem subject = c.create(FirstClassItem.class);
        Thing alias1 = c.create(Thing.class);
        Literal alias2 = c.create(Literal.class);
        Set<Thing> alias = new HashSet<Thing>();
        alias.add(alias1);
        alias.add(alias2);
        subject.setAlias(alias);

        WriteAction action1 = new RemoveAlias(MyOtherBrain.toURI(subject.getQName()),
                MyOtherBrain.toURI(alias1.getQName()));
        WriteAction action2 = new RemoveAlias(MyOtherBrain.toURI(subject.getQName()),
                MyOtherBrain.toURI(alias2.getQName()));
        WriteAction action3 = new RemoveAlias(MyOtherBrain.toURI(subject.getQName()),
                MyOtherBrain.toURI(alias2.getQName()));

        assertEquals(2, subject.getAlias().size());
        assertTrue(subject.getAlias().contains(alias1));
        assertTrue(subject.getAlias().contains(alias2));

        action1.redo(c);
        assertEquals(1, subject.getAlias().size());
        assertTrue(subject.getAlias().contains(alias2));

        action1.undo(c);
        assertEquals(2, subject.getAlias().size());
        assertTrue(subject.getAlias().contains(alias1));
        assertTrue(subject.getAlias().contains(alias2));

        action1.redo(c);
        action2.redo(c);
        assertEquals(0, subject.getAlias().size());

        action3.redo(c);
        assertEquals(0, subject.getAlias().size());

        action2.undo(c);
        action1.undo(c);
        assertEquals(2, subject.getAlias().size());
        assertTrue(subject.getAlias().contains(alias1));
        assertTrue(subject.getAlias().contains(alias2));

        c.rollback();
        c.close();
    }
}