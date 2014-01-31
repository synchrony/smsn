package net.fortytwo.extendo.rfid;

import com.alien.enterpriseRFID.discovery.AlienDiscoverySocketException;
import com.alien.enterpriseRFID.discovery.AlienDiscoveryUnknownReaderException;
import com.alien.enterpriseRFID.discovery.DiscoveryItem;
import com.alien.enterpriseRFID.discovery.DiscoveryListener;
import com.alien.enterpriseRFID.discovery.NetworkDiscoveryListenerService;
import com.alien.enterpriseRFID.reader.AlienClass1Reader;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RFIDListener implements DiscoveryListener {
    private static final Logger LOGGER = Logger.getLogger(RFIDListener.class.getName());

    private final Set<AlienClass1Reader> readers;
    private final NetworkDiscoveryListenerService service;

    public RFIDListener() {
        readers = new HashSet<AlienClass1Reader>();

        service = new NetworkDiscoveryListenerService();
        service.setDiscoveryListener(this);
        try {
            service.startService();
        } catch (AlienDiscoverySocketException e) {
            logError(e);
            System.exit(1);
        }
    }

    public void readerAdded(DiscoveryItem item) {
        LOGGER.info("reader added: " + item);

        AlienClass1Reader r;
        try {
            r = item.getReader();
        } catch (AlienDiscoveryUnknownReaderException e) {
            logError(e);
            return;
        }

        readers.add(r);
    }

    public void readerRenewed(final DiscoveryItem item) {
        LOGGER.info("reader renewed: " + item);
    }

    public void readerRemoved(final DiscoveryItem item) {
        LOGGER.info("reader removed: " + item);

        try {
            readers.remove(item.getReader());
        } catch (AlienDiscoveryUnknownReaderException e) {
            logError(e);
        }
    }

    public AlienClass1Reader firstReader() {
        while (0 == readers.size()) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                logError(e);
                System.exit(1);
            }
        }

        service.stopService();

        return readers.iterator().next();
    }

    private void logError(final Exception e) {
        LOGGER.warning("error: " + e);
        e.printStackTrace(System.err);
    }
}
