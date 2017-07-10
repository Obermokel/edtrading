package borg.edtrading;

import borg.edtrading.data.Coord;
import borg.edtrading.eddn.EddnListener;
import borg.edtrading.eddn.EddnReaderThread;
import borg.edtrading.journal.entries.AbstractJournalEntry.Faction;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * FsdJumpListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FsdJumpListener {

    static final Logger logger = LogManager.getLogger(FsdJumpListener.class);

    public static void main(String[] args) throws Exception {
        //FsdJumpCollector fsdJumpCollector = new FsdJumpCollector(new File(System.getProperty("user.home"), "FSDJumpListener"));
        FsdJumpCollector fsdJumpCollector = new FsdJumpCollector(new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\FSDJumpListener"));

        EddnReaderThread eddnReaderThread = new EddnReaderThread();
        eddnReaderThread.addListener(fsdJumpCollector);
        eddnReaderThread.start();

        try {
            System.in.read();
        } catch (IOException e) {
            e.printStackTrace();
        }

        eddnReaderThread.interrupt();
        fsdJumpCollector.flush();
    }

    public static class FsdJumpCollector implements EddnListener {

        private final File baseDir;
        private final SortedMap<String, SystemBgsData> bgsDataBySystem = new TreeMap<>();
        private int counter = 0;

        public FsdJumpCollector(File baseDir) {
            this.baseDir = baseDir;
        }

        @Override
        public void onCommanderLocation(Date timestamp, String commanderName, String systemName, Coord systemCoords, List<Faction> systemFactions) {
            if (systemFactions != null && systemFactions.size() > 0) {
                logger.info("[" + timestamp + "] " + commanderName + " @ " + systemName + ": " + systemFactions);

                // Lookup system
                SystemBgsData systemBgsData = this.bgsDataBySystem.get(systemName.toUpperCase());
                if (systemBgsData == null) {
                    systemBgsData = new SystemBgsData(systemName);
                    this.bgsDataBySystem.put(systemName.toUpperCase(), systemBgsData);
                }

                // Add data
                systemBgsData.addFactionData(timestamp, systemFactions);

                // Periodical flush
                if (++counter % 1000 == 0) {
                    try {
                        this.flush();
                    } catch (IOException e) {
                        logger.error("Failed to flush", e);
                    }
                }
            }
        }

        public void flush() throws IOException {
            if (!baseDir.exists()) {
                baseDir.mkdirs();
            }
            for (String systemName : this.bgsDataBySystem.keySet()) {
                File logFile = new File(baseDir, systemName + ".log");
                if (!logFile.exists()) {
                    FileUtils.write(logFile, "", "UTF-8");
                }
                SystemBgsData systemBgsData = this.bgsDataBySystem.get(systemName);
                for (Date timestamp : systemBgsData.getFactionsByTimestamp().keySet()) {
                    StringBuilder line = new StringBuilder(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(timestamp));
                    for (Faction faction : systemBgsData.getFactionsByTimestamp().get(timestamp)) {
                        line.append(", ");
                        line.append(String.format(Locale.US, "%s|%s|%.1f", faction.getName(), faction.getFactionState(), faction.getInfluence() * 100).replace(",", "_"));
                    }
                    line.append("\n");
                    FileUtils.write(logFile, line, "UTF-8", true);
                }
            }
            this.bgsDataBySystem.clear();
        }

    }

    public static class SystemBgsData implements Serializable {

        private static final long serialVersionUID = -6386799862567873524L;

        private final String systemName;
        private final SortedMap<Date, List<Faction>> factionsByTimestamp;

        public SystemBgsData(String systemName) {
            this.systemName = systemName;
            this.factionsByTimestamp = new TreeMap<>();
        }

        @Override
        public String toString() {
            return String.format(Locale.US, "%-40s", this.systemName);
        }

        public void addFactionData(Date timestamp, List<Faction> systemFactions) {
            this.factionsByTimestamp.put(timestamp, systemFactions);
        }

        public String getSystemName() {
            return this.systemName;
        }

        public SortedMap<Date, List<Faction>> getFactionsByTimestamp() {
            return this.factionsByTimestamp;
        }

    }

}
