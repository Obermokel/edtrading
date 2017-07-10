package borg.edtrading.eddn;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.AbstractJournalEntry.Faction;
import borg.edtrading.journal.entries.exploration.ScanEntry;
import borg.edtrading.journal.entries.location.DockedEntry;
import borg.edtrading.journal.entries.location.FSDJumpEntry;
import borg.edtrading.journal.entries.location.LocationEntry;
import borg.edtrading.util.MiscUtil;
import com.google.gson.Gson;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.zeromq.ZMQ;
import zmq.ZError;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.channels.ClosedByInterruptException;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

/**
 * EddnReaderThread
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddnReaderThread extends Thread {

    static final Logger logger = LogManager.getLogger(EddnReaderThread.class);

    private final List<EddnListener> listeners = new ArrayList<>();

    public EddnReaderThread() {
        super();

        this.setName("EddnReaderThread");
        this.setDaemon(false);
    }

    @Override
    public void run() {
        logger.info(this.getName() + " started");

        JournalReader journalReader = new JournalReader();
        Gson gson = new Gson();

        ZMQ.Context context = null;
        ZMQ.Socket socket = null;

        try {
            context = ZMQ.context(1);

            socket = context.socket(ZMQ.SUB);
            socket.subscribe(new byte[0]);
            socket.setReceiveTimeOut(600000);
            socket.connect("tcp://eddn-relay.elite-markets.net:9500");

            while (!Thread.currentThread().isInterrupted()) {
                byte[] compressed = socket.recv(0);

                if (compressed == null) {
                    // Reconnect
                    try {
                        if (socket != null) {
                            logger.debug("Closing socket after receiving null");
                            socket.close();
                        }
                    } catch (Exception e) {
                        logger.error("Failed to close 0MQ socket", e);
                    }
                    try {
                        if (context != null) {
                            logger.debug("Terminating context after receiving null");
                            context.term();
                        }
                    } catch (Exception e) {
                        // Ignore
                    }

                    logger.debug("Re-instantiating context after receiving null");
                    context = ZMQ.context(1);

                    logger.debug("Opening socket after receiving null");
                    socket = context.socket(ZMQ.SUB);
                    socket.subscribe(new byte[0]);
                    socket.setReceiveTimeOut(600000);
                    socket.connect("tcp://eddn-relay.elite-markets.net:9500");
                } else {
                    byte[] decompressed = decompress(compressed);
                    String json = new String(decompressed, "UTF-8");

                    try {
                        LinkedHashMap<String, Object> data = gson.fromJson(json, LinkedHashMap.class);
                        String schemaRef = MiscUtil.getAsString(data.get("$schemaRef"));
                        if ("http://schemas.elite-markets.net/eddn/journal/1".equals(schemaRef)) {
                            String uploaderId = MiscUtil.getAsString(((Map<String, Object>) data.get("header")).get("uploaderID"));
                            String systemName = null;
                            Coord systemCoords = null;
                            List<Faction> systemFactions = null;
                            LinkedHashMap<String, Object> journalMessage = new LinkedHashMap<String, Object>((Map<String, Object>) data.get("message"));
                            AbstractJournalEntry journalData = journalReader.readJournalData(journalMessage);
                            Date timestamp = journalData.getTimestamp();

                            if (journalData.getEvent() == Event.Location) {
                                systemName = ((LocationEntry) journalData).getStarSystem();
                                systemCoords = ((LocationEntry) journalData).getStarPos();
                                systemFactions = ((LocationEntry) journalData).getFactions();
                            } else if (journalData.getEvent() == Event.FSDJump) {
                                systemName = ((FSDJumpEntry) journalData).getStarSystem();
                                systemCoords = ((FSDJumpEntry) journalData).getStarPos();
                                systemFactions = ((FSDJumpEntry) journalData).getFactions();
                            } else if (journalData.getEvent() == Event.Docked) {
                                systemName = ((DockedEntry) journalData).getStarSystem();
                                systemCoords = ((DockedEntry) journalData).getStarPos();
                                systemFactions = ((DockedEntry) journalData).getFactions();
                            } else if (journalData.getEvent() == Event.Scan) {
                                systemName = ((ScanEntry) journalData).getStarSystem();
                                systemCoords = ((ScanEntry) journalData).getStarPos();
                            }

                            if (timestamp != null && StringUtils.isNotEmpty(uploaderId) && StringUtils.isNotEmpty(systemName) && systemCoords != null) {
                                for (EddnListener listener : this.listeners) {
                                    try {
                                        listener.onCommanderLocation(timestamp, uploaderId, systemName, systemCoords, systemFactions);
                                    } catch (Exception ex) {
                                        logger.warn(listener + " failed: " + ex);
                                    }
                                }
                            }
                        }
                    } catch (Exception e) {
                        logger.error("Failed to process received JSON '" + json + "'", e);
                    }
                }
            }
        } catch (Exception e) {
            if (e instanceof ZError.IOException && e.getCause() instanceof ClosedByInterruptException) {
                // Closed
            } else {
                logger.fatal("EddnReaderThread crashed", e);
            }
        } finally {
            try {
                if (socket != null) {
                    socket.close();
                }
            } catch (Exception e) {
                logger.error("Failed to close 0MQ socket", e);
            }
            try {
                if (context != null) {
                    context.term();
                }
            } catch (Exception e) {
                // Ignore
            }
        }

        logger.info(this.getName() + " stopped");
    }

    private static byte[] decompress(byte[] data) throws IOException, DataFormatException {
        Inflater inflater = new Inflater();
        inflater.setInput(data);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream(data.length);
        byte[] buffer = new byte[1024];
        while (!inflater.finished()) {
            int count = inflater.inflate(buffer);
            outputStream.write(buffer, 0, count);
        }
        outputStream.close();
        return outputStream.toByteArray();
    }

    public boolean addListener(EddnListener listener) {
        if (listener == null || this.listeners.contains(listener)) {
            return false;
        } else {
            return this.listeners.add(listener);
        }
    }

    public boolean removeListener(EddnListener listener) {
        if (listener == null) {
            return false;
        } else {
            return this.listeners.remove(listener);
        }
    }

}
