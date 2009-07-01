package net.fortytwo.myotherbrain.access;

import net.fortytwo.myotherbrain.MOBModel;
import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.MOBStore;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.access.error.BadEmailAddressException;
import net.fortytwo.myotherbrain.access.error.BadPasswordException;
import net.fortytwo.myotherbrain.access.error.BadUserNameException;
import net.fortytwo.myotherbrain.access.error.EmailAddressIsAlreadyInUseException;
import net.fortytwo.myotherbrain.access.error.EmailAddressIsInvalidException;
import net.fortytwo.myotherbrain.access.error.EmailAddressIsTooLongException;
import net.fortytwo.myotherbrain.access.error.NoSuchAccountException;
import net.fortytwo.myotherbrain.access.error.PasswordIsInvalidException;
import net.fortytwo.myotherbrain.access.error.PasswordIsTooLongException;
import net.fortytwo.myotherbrain.access.error.PasswordIsTooShortException;
import net.fortytwo.myotherbrain.access.error.UserNameIsAlreadyInUseException;
import net.fortytwo.myotherbrain.access.error.UserNameIsInappropriateException;
import net.fortytwo.myotherbrain.access.error.UserNameIsInvalidException;
import net.fortytwo.myotherbrain.access.error.UserNameIsReservedException;
import net.fortytwo.myotherbrain.access.error.UserNameIsTooLongException;
import net.fortytwo.myotherbrain.access.error.UserNameIsTooShortException;
import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.model.beans.Account;
import net.fortytwo.myotherbrain.model.beans.Graph;
import net.fortytwo.myotherbrain.tools.properties.PropertyException;
import net.fortytwo.myotherbrain.tools.properties.TypedProperties;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.ElmoQuery;
import org.openrdf.model.URI;

import java.net.URISyntaxException;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;


/**
 * Author: josh
 * Date: Mar 11, 2009
 * Time: 1:40:13 PM
 */
public class AccountManager {

    private static final int
            USERNAME_MINIMUM_LENGTH,
            USERNAME_MAXIMUM_LENGTH,
            PASSWORD_MINIMUM_LENGTH,
            PASSWORD_MAXIMUM_LENGTH,
            EMAILADDRESS_MAXIMUM_LENGTH;

    private static final Pattern
            USERNAME_PATTERN,
            PASSWORD_PATTERN,
            EMAIL_PATTERN;

    static {
        String base = "net.fortytwo.myotherbrain.access.";
        TypedProperties p = MyOtherBrain.getProperties();
        try {
            USERNAME_MINIMUM_LENGTH = p.getInt(base + "userNameMinimumLength");
            USERNAME_MAXIMUM_LENGTH = p.getInt(base + "userNameMaximumLength");
            PASSWORD_MINIMUM_LENGTH = p.getInt(base + "passwordMinimumLength");
            PASSWORD_MAXIMUM_LENGTH = p.getInt(base + "passwordMaximumLength");
            EMAILADDRESS_MAXIMUM_LENGTH = p.getInt(base + "emailAddressMaximumLength");
        } catch (PropertyException e) {
            throw new ExceptionInInitializerError(e);
        }

        // Note: these patterns aren't read from a properties file for reasons
        // of convenience and because they are Java-specific.
        USERNAME_PATTERN = Pattern.compile("[a-zA-Z][a-zA-Z0-9]*");
        PASSWORD_PATTERN = Pattern.compile("[\\x21-\\x7e]+");

        // See: http://www.regular-expressions.info/email.html
        // This is the "more practical implementation of RFC 2822" omitting the
        // syntax using double quotes and square brackets.
        // It has been modified to include uppercase characters in the local part.
        // Uppercase characters are not allowed in the domain part, as this is
        // expected to have been normalized to lowercase.
        EMAIL_PATTERN = Pattern.compile("[a-z0-9&&[^#$%&'*+/=?^_`{|}~-]]+" +
                "(\\.[a-zA-Z0-9&&[^#$%&'*+/=?^_`{|}~-]]+)*" +
                "@" +
                "([a-z0-9]([a-z0-9-]*[a-z0-9])?\\.)+" +
                "[a-z0-9]" +
                "([a-z0-9-]*[a-z0-9])?");
    }

    private final MOBModel adminModel;

    public AccountManager(final MOBStore store) {
        URI adminGraph = store.getSail().getValueFactory().createURI(MOB.MOBADMINGRAPH);
        adminModel = store.createModel(adminGraph);
    }

    // For unit testing purposes.
    MOBModel getModel() {
        return adminModel;
    }

    /**
     * Produces an estimate of the "strength" of a password, to be made when a
     * user first proposes a password for a new account.
     *
     * @param password the password to check
     * @return a value between 0.0 and 1.0 indicating the "strength" of the
     *         password
     */
    public double passwordStrength(final String password) {
        // FIXME
        return 0.0;
    }

    /**
     * Checks whether the given String will be accepted as a user name for a new
     * account, throwing an Exception if not.  This allows the service to report
     * a bad user name when the user first proposes it, rather than failing on
     * actual account creation.  It is possible that a user name which passes
     * this check is not accepted for a new account: that is, if the user name
     * has been claimed in the time between the check and the attempt to create
     * an account.
     *
     * @param name the user name to check
     * @throws BadUserNameException if the user name is not accepted
     */
    public void checkUserName(final String name) throws BadUserNameException {
        if (userNameIsTooShort(name)) {
            throw new UserNameIsTooShortException("user name is too short");
        } else if (userNameIsTooLong(name)) {
            throw new UserNameIsTooLongException("user name is too long");
        } else if (userNameIsSyntacticallyInvalid(name)) {
            // TODO: it may be necessary to further break this down into "forbidden characters" vs. additional constraints
            throw new UserNameIsInvalidException("user name has invalid syntax");
        } else if (userNameIsReserved(name)) {
            throw new UserNameIsReservedException("user name has been reserved");
        } else if (userNameIsInappropriate(name)) {
            // TODO: insert actual MOB support email address.
            throw new UserNameIsInappropriateException("user name has been flagged as inappropriate. Please contact an administrator if you feel this name should have been accepted.");
        }
    }

    /**
     * Checks whether the given String will be accepted as a password for a new
     * account, throwing an Exception if not.  This allows the service to report
     * a bad password when the user first proposes it, rather than failing on
     * actual account creation.
     *
     * @param password the password to check
     * @throws BadPasswordException if the password is not accepted
     */
    public void checkPassword(final String password) throws BadPasswordException {
        if (passwordIsTooShort(password)) {
            throw new PasswordIsTooShortException("password is too short");
        } else if (passwordIsTooLong(password)) {
            throw new PasswordIsTooLongException("password is too long");
        } else if (passwordIsSyntacticallyInvalid(password)) {
            throw new PasswordIsInvalidException("password contains forbidden characters");
        }
    }

    public void checkContactEmailAddress(final String emailAddress) throws BadEmailAddressException {
        if (emailAddressIsTooLong(emailAddress)) {
            throw new EmailAddressIsTooLongException("email address is too long");
        } else if (emailAddressIsSyntacticallyInvalid(emailAddress)) {
            throw new EmailAddressIsInvalidException("email address has invalid syntax");
        }
    }

    /**
     * This method is synchronized application-wide to avoid a race condition in
     * which two users are granted the same user name.
     *
     * @param userName            the desired user name
     * @param password            an unencrypted, unhashed password (it will be hashed for storage)
     * @param contactEmailAddress a valid contact email address
     * @throws net.fortytwo.myotherbrain.access.error.BadEmailAddressException
     *          if the supplied email address cannot be accepted
     * @throws net.fortytwo.myotherbrain.access.error.BadPasswordException
     *          if the supplied password cannot be accepted
     * @throws net.fortytwo.myotherbrain.access.error.BadUserNameException
     *          if the supplied user name cannot be accepted
     */
    public synchronized void createAccount(final String userName,
                                           final String password,
                                           final String contactEmailAddress) throws BadPasswordException, BadUserNameException, BadEmailAddressException {
        // TODO: validate contact email address by sending an email containing a confirmation URL
        MOBModelConnection c = adminModel.createConnection();
        try {
            // Note: error messages for syntactically invalid passwords and user
            // names assume that the user has already been told what constitutes a
            // valid password and user name.
            // TODO: multilingual error messages
            checkUserName(userName);
            if (userNameIsAlreadyInUse(userName, c)) {
                throw new UserNameIsAlreadyInUseException("user name is already in use");
            }
            checkPassword(password);
            checkContactEmailAddress(contactEmailAddress);
            if (contactEmailAddressIsAlreadyInUse(contactEmailAddress, c)) {
                throw new EmailAddressIsAlreadyInUseException("email address is already in use");
            }

            String timeStamp = currentTimeStamp();

            Graph personalGraph = c.create(Graph.class);
            logComment(personalGraph, "personal graph created for user '" + userName + "' on " + timeStamp);

            Account account = c.create(Account.class);
            logComment(account, "account created for user '" + userName + "' on " + timeStamp);
            account.setUserName(userName);
            account.setPasswordSha1Sum(MyOtherBrain.sha1SumOf(password));
            account.setContactEmailAddress(emailAddressURI(contactEmailAddress));
            account.setPersonalGraph(personalGraph);

            c.commit();
        } finally {
            c.close();
        }
    }

    // TODO: return an account object
    public void authenticate(final String userName,
                             final String password) {

    }

    /**
     * This method is synchronized to avoid a race condition w.r.t. the desired
     * user name.
     *
     * @param currentName the user's current name
     * @param newName     must be different than oldName
     * @throws net.fortytwo.myotherbrain.access.error.BadUserNameException
     *          if the desired user name is invalid
     * @throws net.fortytwo.myotherbrain.access.error.NoSuchAccountException
     *          if an account with the 'current' user name does not exist
     */
    public synchronized void changeUserName(final String currentName,
                                            final String newName) throws BadUserNameException, NoSuchAccountException {
        if (currentName.equals(newName)) {
            throw new IllegalArgumentException("new name must be different than current name");
        }

        try {
            checkUserName(currentName);
        } catch (BadUserNameException e) {
            throw new IllegalArgumentException("current name is not valid");
        }

        checkUserName(newName);

        MOBModelConnection c = adminModel.createConnection();
        try {
            if (userNameIsAlreadyInUse(newName, c)) {
                throw new UserNameIsAlreadyInUseException("user name is already in use");
            }

            Account account = getAccountByUserName(currentName, c);
            if (null == account) {
                throw new NoSuchAccountException(currentName);
            }
            account.setUserName(newName);
            logComment(account, "user name changed to '" + newName + "' on " + currentTimeStamp());

            c.commit();
        } finally {
            c.close();
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    private String currentTimeStamp() {
        // TODO: custom formatting of date/time for admin comments
        return new Date().toString();
    }

    private java.net.URI emailAddressURI(final String address) {
        try {
            return new java.net.URI("mailto:" + address);
        } catch (URISyntaxException e) {
            throw new IllegalArgumentException("invalid email address");
        }
    }

    // TODO: is there any way to avoid re-compile these queries for each use?
    private static final String
            SELECT_ACCOUNT_BY_USERNAME = "SELECT ?account\n"
                + " WHERE { ?account <" + MOB.USERNAME + "> ?userName . }",
            SELECT_ACCOUNT_BY_CONTACTEMAILADDRESS = "SELECT ?account\n"
                + "WHERE { ?account <" + MOB.CONTACTEMAILADDRESS + "> ?contactEmailAddress . }";
    
    private Account getAccountByUserName(final String userName,
                                         final MOBModelConnection c) {
        ElmoQuery query = c.getElmoManager().createQuery(SELECT_ACCOUNT_BY_USERNAME);

        query.setParameter("userName", userName);
        List results = query.getResultList();

        // Note: we don't check for duplicate user names here.
        return (0 == results.size())
                ? null
                : (Account) results.iterator().next();
    }

    private Account getAccountByContactEmailAddress(final String contactEmailAddress,
                                                    final MOBModelConnection c) {
        ElmoQuery query = c.getElmoManager().createQuery(SELECT_ACCOUNT_BY_CONTACTEMAILADDRESS);

        query.setParameter("contactEmailAddress", emailAddressURI(contactEmailAddress));
        List results = query.getResultList();

        // Note: we don't check for duplicate contact email addresses here.
        return (0 == results.size())
                ? null
                : (Account) results.iterator().next();
    }

    private void logComment(final Thing t,
                            final String comment) {
        String s = t.getRdfsComment();
        if (null == s) {
            s = comment;
        } else {
            s = s + ";\n" + comment;
        }
        t.setRdfsComment(s);
    }

    ////////////////////////////////////////////////////////////////////////////

    private boolean userNameIsTooShort(final String name) {
        return name.length() < USERNAME_MINIMUM_LENGTH;
    }

    private boolean userNameIsTooLong(final String name) {
        return name.length() > USERNAME_MAXIMUM_LENGTH;
    }

    private boolean userNameIsSyntacticallyInvalid(final String name) {
        return !USERNAME_PATTERN.matcher(name).matches();
    }

    private boolean userNameIsReserved(final String name) {
        // FIXME
        return false;
    }

    private boolean userNameIsInappropriate(final String name) {
        // FIXME
        return false;
    }

    private boolean passwordIsTooShort(final String password) {
        return password.length() < PASSWORD_MINIMUM_LENGTH;
    }

    private boolean passwordIsTooLong(final String password) {
        return password.length() > PASSWORD_MAXIMUM_LENGTH;
    }

    private boolean passwordIsSyntacticallyInvalid(final String password) {
        return !PASSWORD_PATTERN.matcher(password).matches();
    }

    private boolean emailAddressIsTooLong(final String emailAddress) {
        return emailAddress.length() > EMAILADDRESS_MAXIMUM_LENGTH;
    }

    private boolean emailAddressIsSyntacticallyInvalid(final String emailAddress) {
        return !EMAIL_PATTERN.matcher(emailAddress).matches();
    }

    ////////////////////////////////////////////////////////////////////////////

    private boolean userNameIsAlreadyInUse(final String userName,
                                           final MOBModelConnection c) {
        return (null != getAccountByUserName(userName, c));
    }

    private boolean contactEmailAddressIsAlreadyInUse(final String contactEmailAddress,
                                                      final MOBModelConnection c) {
        return (null != getAccountByContactEmailAddress(contactEmailAddress, c));
    }
}
