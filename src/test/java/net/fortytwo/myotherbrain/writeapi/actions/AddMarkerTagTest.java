package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.Marker;

import javax.xml.namespace.QName;

/**
 * Author: josh
 * Date: Jul 3, 2009
 * Time: 12:00:01 PM
 */
public class AddMarkerTagTest extends WriteActionTestCase {

    public void testAll() throws Exception {
        MOBModelConnection c = model.createConnection();

        FirstClassItem subject = c.create(FirstClassItem.class);
        Marker tag1 = c.create(new QName(MOB.STARRED), Marker.class);
        Marker tag2 = c.create(new QName(MOB.BROKEN), Marker.class);

        WriteAction action1 = new AddMarkerTag(MyOtherBrain.toURI(subject.getQName()),
                MyOtherBrain.toURI(tag1.getQName()));
        WriteAction action2 = new AddMarkerTag(MyOtherBrain.toURI(subject.getQName()),
                MyOtherBrain.toURI(tag2.getQName()));

        assertEquals(0, subject.getMarkerTag().size());

        action1.redo(c);
        assertEquals(1, subject.getMarkerTag().size());
        assertTrue(subject.getMarkerTag().contains(tag1));

        action1.undo(c);
        assertEquals(0, subject.getMarkerTag().size());

        action1.redo(c);
        action2.redo(c);
        assertEquals(2, subject.getMarkerTag().size());
        assertTrue(subject.getMarkerTag().contains(tag1));
        assertTrue(subject.getMarkerTag().contains(tag2));

        action2.undo(c);
        assertEquals(1, subject.getMarkerTag().size());
        assertTrue(subject.getMarkerTag().contains(tag1));
        
        action1.undo(c);
        assertEquals(0, subject.getMarkerTag().size());

        c.rollback();
        c.close();
    }
}