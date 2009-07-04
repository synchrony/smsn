package net.fortytwo.myotherbrain.access;

import junit.framework.TestCase;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.MOBStore;
import net.fortytwo.myotherbrain.access.error.EmailAddressIsInvalidException;
import net.fortytwo.myotherbrain.access.error.EmailAddressIsTooLongException;
import net.fortytwo.myotherbrain.access.error.PasswordIsInvalidException;
import net.fortytwo.myotherbrain.access.error.PasswordIsTooLongException;
import net.fortytwo.myotherbrain.access.error.PasswordIsTooShortException;
import net.fortytwo.myotherbrain.access.error.UserNameIsInvalidException;
import net.fortytwo.myotherbrain.access.error.UserNameIsTooLongException;
import net.fortytwo.myotherbrain.access.error.UserNameIsTooShortException;
import org.openrdf.sail.Sail;
import org.openrdf.sail.memory.MemoryStore;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Author: josh
 * Date: Jun 30, 2009
 * Time: 9:43:15 PM
 */
public class AccountManagerTest extends TestCase {
    private Sail sail;
    private MOBStore store;
    private AccountManager manager;
    private MOBModelConnection connection;

    public void setUp() throws Exception {
        sail = new MemoryStore();
        sail.initialize();
        store = new MOBStore(sail);
        store.initialize();
        manager = new AccountManager(store);
        connection = manager.getModel().createConnection();
    }

    public void tearDown() throws Exception {
        connection.rollback();
        connection.close();
        store.shutDown();
        sail.shutDown();
    }

    public void testCheckUserName() throws Exception {
        Method m = AccountManager.class.getMethod("checkUserName", String.class);

        assertNull(thrown(m, "a"));
        assertNull(thrown(m, "johnny"));
        assertNull(thrown(m, "MixedCase"));
        assertNull(thrown(m, "crazy88"));

        assertEquals(UserNameIsTooShortException.class, thrown(m, ""));
        assertEquals(UserNameIsTooLongException.class, thrown(m, "thisusernameisdefinitelyalittletoolong"));
        assertEquals(UserNameIsInvalidException.class, thrown(m, "4cantstartwithnumber"));
        assertEquals(UserNameIsInvalidException.class, thrown(m, "bad_characters!"));

        // TODO: reserved user name
        // TODO: inappropriate user name

        // TODO: user name already in use
    }

    public void testCheckPassword() throws Exception {
        Method m = AccountManager.class.getMethod("checkPassword", String.class);

        assertNull(thrown(m, "goodPassword"));
        assertNull(thrown(m, "MixedCaseIsOK"));
        assertNull(thrown(m, "!special-=$%^chars-are-OK"));

        assertEquals(PasswordIsTooShortException.class, thrown(m, ""));
        assertEquals(PasswordIsTooShortException.class, thrown(m, "bad"));
        assertEquals(PasswordIsTooLongException.class, thrown(m, "thispasswordisjusttoodarnedlong.don'tyouagree"));
        assertEquals(PasswordIsInvalidException.class, thrown(m, "no spaces"));
    }

    public void testCheckContactEmailAddress() throws Exception {
        Method m = AccountManager.class.getMethod("checkContactEmailAddress", String.class);

        assertNull(thrown(m, "a@b.ch"));
        assertNull(thrown(m, "bob.dobbs@example.org"));
        assertNull(thrown(m, "bob.dobbs@crazy.tld"));
        assertNull(thrown(m, "foo@subdomains.are.ok"));

        assertEquals(EmailAddressIsTooLongException.class, thrown(m, "ThisEmailAddressDefinitelySeemsJustALittleTooLong@example.org"));
        assertEquals(EmailAddressIsInvalidException.class, thrown(m, "@gmail.com"));
        assertEquals(EmailAddressIsInvalidException.class, thrown(m, "bob@ex!!!ample.org"));
        assertEquals(EmailAddressIsInvalidException.class, thrown(m, "domain-part@ShouldBeNormalized.org"));

        // TODO: email address already in use
    }

    private Class thrown(final Method method,
                         final Object... args) throws IllegalAccessException {
        try {
            method.invoke(manager, args);
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            return cause.getClass();
        }
        return null;
    }
}
