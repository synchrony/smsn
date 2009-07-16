package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.model.concepts.Marker;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;

import javax.xml.namespace.QName;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: josh
 * Date: Jul 3, 2009
 * Time: 12:00:01 PM
 */
public class RemoveMarkerTagTest extends WriteActionTestCase {

    public void testAll() throws Exception {
        WriteContext c = new WriteContext(model.createConnection());

        FirstClassItem subject = c.create(FirstClassItem.class);
        Marker tag1 = c.create(new QName(MOB.STARRED), Marker.class);
        Marker tag2 = c.create(new QName(MOB.BROKEN), Marker.class);
        Set<Marker> markerTag = new HashSet<Marker>();
        markerTag.add(tag1);
        markerTag.add(tag2);
        subject.setMarkerTag(markerTag);

        WriteAction action1 = new RemoveMarkerTag(MyOtherBrain.toURI(subject.getQName()),
                MyOtherBrain.toURI(tag1.getQName()), c);
        WriteAction action2 = new RemoveMarkerTag(MyOtherBrain.toURI(subject.getQName()),
                MyOtherBrain.toURI(tag2.getQName()), c);
        WriteAction action3 = new RemoveMarkerTag(MyOtherBrain.toURI(subject.getQName()),
                MyOtherBrain.toURI(tag2.getQName()), c);

        assertEquals(2, subject.getMarkerTag().size());
        assertTrue(subject.getMarkerTag().contains(tag1));
        assertTrue(subject.getMarkerTag().contains(tag2));

        action1.redo(c);
        assertEquals(1, subject.getMarkerTag().size());
        assertTrue(subject.getMarkerTag().contains(tag2));

        action1.undo(c);
        assertEquals(2, subject.getMarkerTag().size());
        assertTrue(subject.getMarkerTag().contains(tag1));
        assertTrue(subject.getMarkerTag().contains(tag2));

        action1.redo(c);
        action2.redo(c);
        assertEquals(0, subject.getMarkerTag().size());

        action3.redo(c);
        assertEquals(0, subject.getMarkerTag().size());

        action2.undo(c);
        action1.undo(c);
        assertEquals(2, subject.getMarkerTag().size());
        assertTrue(subject.getMarkerTag().contains(tag1));
        assertTrue(subject.getMarkerTag().contains(tag2));

        c.getConnection().rollback();
        c.getConnection().close();
    }
}