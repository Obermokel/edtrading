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
        FsdJumpCollector fsdJumpCollector = new FsdJumpCollector();

        EddnReaderThread eddnReaderThread = new EddnReaderThread();
        eddnReaderThread.addListener(fsdJumpCollector);
        eddnReaderThread.start();

        try {
            System.in.read();
        } catch (IOException e) {
            e.printStackTrace();
        }

        eddnReaderThread.interrupt();

        //File baseDir = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Factions\\BGS Journal");
        File baseDir = new File(System.getProperty("user.home"), "FSDJumpListener");
        if (!baseDir.exists()) {
            baseDir.mkdirs();
        }
        for (String key : fsdJumpCollector.getBgsDataBySystem().keySet()) {
            File logFile = new File(baseDir, key + ".log");
            if (!logFile.exists()) {
                FileUtils.write(logFile, "", "UTF-8");
            }
            SystemBgsData systemBgsData = fsdJumpCollector.getBgsDataBySystem().get(key);
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
    }

    public static class FsdJumpCollector implements EddnListener {

        private final SortedMap<String, SystemBgsData> bgsDataBySystem = new TreeMap<>();

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
            }
        }

        public SortedMap<String, SystemBgsData> getBgsDataBySystem() {
            return this.bgsDataBySystem;
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
