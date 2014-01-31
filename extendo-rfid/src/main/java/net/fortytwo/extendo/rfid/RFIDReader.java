package net.fortytwo.extendo.rfid;

import com.alien.enterpriseRFID.reader.AlienClass1Reader;
import com.alien.enterpriseRFID.reader.AlienReaderException;
import com.alien.enterpriseRFID.tags.Tag;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RFIDReader {
    private static final Logger LOGGER = Logger.getLogger(RFIDReader.class.getName());

    private final AlienClass1Reader alienReader;
    private final int numberOfAntennas;
    private final PrintStream log;

    private final Set<String> tagIds;

    public RFIDReader(final AlienClass1Reader alienReader,
                      final int numberOfAntennas,
                      final PrintStream log) throws AlienReaderException {
        this.alienReader = alienReader;

        /*
        System.out.println("##### maxAntenna = " + this.alienReader.getMaxAntenna());
        System.out.println("##### antennaSequence = " + this.alienReader.getAntennaSequence());
        for (int i = 0; i < 4; i++) {
            System.out.println("##### level " + i + ": " + this.alienReader.getRFLevel(i));
            System.out.println("##### attenuation " + i + ": " + this.alienReader.getRFAttenuation(i));
        }
        System.out.println("##### info: " + this.alienReader.getInfo());
        System.out.println("##### tag list + antenna: " + this.alienReader.getTagListAntennaCombine());
        */

        if (1 > numberOfAntennas) {
            throw new IllegalArgumentException("can't have less than one antenna");
        } else if (1 == numberOfAntennas) {
            alienReader.setAntennaSequence("0");
        }
        this.numberOfAntennas = numberOfAntennas;


        LOGGER.info("reader: " + alienReader);
        alienReader.setUsername("alien");
        alienReader.setPassword("password");

        alienReader.setTagType(AlienClass1Reader.CLASS1GEN2);
        this.log = log;

        tagIds = new HashSet<String>();

        if (null != log) {
            log.println("id,antenna,crc,direction,discoverTime,hostDiscoverTime,hostRenewTime," +
                    "persistTime,protocol,protocolString,receiveAntenna,renewCount,renewTime,rssi," +
                    "smoothPosition,smoothSpeed,speed,timeToLive,transmitAntenna");
        }
    }

    private final Random random = new Random();

    // note: only length-2 sequences are created, as Alien seems to ignore third and higher antennas in a sequence
    private void randomAntennaSequence() throws AlienReaderException {
        if (1 != numberOfAntennas) {
            int a1 = random.nextInt(numberOfAntennas);
            int a2 = random.nextInt(numberOfAntennas);
            alienReader.setAntennaSequence("" + a1 + "," + a2);
            LOGGER.fine("setting antenna sequence to " + a1 + "," + a2);
            //alienReader.setAntennaSequence("" + a1);
        }
    }

    public Set<String> getTagIds() {
        return tagIds;
    }

    public void clear() {
        tagIds.clear();
    }

    public void readTags() throws AlienReaderException {
        // This avoids "broken pipe" errors
        alienReader.close();
        alienReader.open();

        long before = System.currentTimeMillis();
        randomAntennaSequence();
        Tag[] tagList = alienReader.getTagList();
        long after = System.currentTimeMillis();
        // this takes 370s +/- 160s for Wowbagger (an ALR-9800) with four antennas and default settings
        LOGGER.fine("finished reading tags in " + (after - before) + "ms");

        if (null != tagList) { // the API does sometimes return a null tag list
            for (Tag t : tagList) {
                tagIds.add(t.getTagID());

                if (null != log) {
                    StringBuilder sb = new StringBuilder();
                    sb.append(t.getTagID())
                            .append(",").append(t.getAntenna())
                            .append(",").append(t.getCRC())
                            .append(",").append(t.getDirection())
                            .append(",").append(t.getDiscoverTime())
                            // TODO: G2 data?
                            .append(",").append(t.getHostDiscoverTime())
                            .append(",").append(t.getHostRenewTime())
                            .append(",").append(t.getPersistTime())
                            .append(",").append(t.getProtocol())
                            .append(",").append(t.getProtocolString())
                            .append(",").append(t.getReceiveAntenna())
                            .append(",").append(t.getRenewCount())
                            .append(",").append(t.getRenewTime())
                            .append(",").append(t.getRSSI())
                            .append(",").append(t.getSmoothPosition())
                            .append(",").append(t.getSmoothSpeed())
                            .append(",").append(t.getSpeed())
                            .append(",").append(t.getTimeToLive())
                            .append(",").append(t.getTransmitAntenna());
                    log.println(sb);
                }
            }
        }
    }
}
