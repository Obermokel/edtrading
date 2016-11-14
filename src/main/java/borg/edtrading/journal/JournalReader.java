package borg.edtrading.journal;

import borg.edtrading.util.MiscUtil;
import com.google.gson.Gson;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.lang3.StringUtils;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

/**
 * Not thread safe
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class JournalReader {

    static final Logger logger = LogManager.getLogger(JournalReader.class);

    private final DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
    private final Gson gson = new Gson();

    public List<AbstractJournalEntry> readEntireJournal(File journalDir) throws IOException {
        List<AbstractJournalEntry> result = new ArrayList<>();

        File[] journalFiles = journalDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().startsWith("Journal.") && file.getName().endsWith(".log");
            }
        });
        Arrays.sort(journalFiles, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return new Long(f1.lastModified()).compareTo(new Long(f2.lastModified()));
            }
        });

        LinkedHashMap<String, Integer> unknownEventCounts = new LinkedHashMap<>();
        for (File journalFile : journalFiles) {
            result.addAll(this.readJournalFile(journalFile, unknownEventCounts));
        }

        MiscUtil.sortMapByValue(unknownEventCounts);
        for (String eventName : unknownEventCounts.keySet()) {
            logger.debug(String.format(Locale.US, "Unknown event: %4dx %s", unknownEventCounts.get(eventName), eventName));
        }

        return result;
    }

    public List<AbstractJournalEntry> readJournalFile(File journalFile, LinkedHashMap<String, Integer> unknownEventCounts) throws IOException {
        List<AbstractJournalEntry> result = new ArrayList<>();

        List<String> lines = FileUtils.readLines(journalFile, "UTF-8");
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            try {
                AbstractJournalEntry entry = this.readJournalLine(line);
                if (entry != null) {
                    result.add(entry);
                }
            } catch (UnknownEventException e) {
                logger.trace("Unknown event type '" + e.getEvent() + "' in line " + (i + 1) + " of " + journalFile);
                Integer count = unknownEventCounts.get(e.getEvent());
                if (count == null) {
                    count = new Integer(0);
                    unknownEventCounts.put(e.getEvent(), count);
                }
                unknownEventCounts.put(e.getEvent(), unknownEventCounts.get(e.getEvent()) + 1);
            } catch (Exception e) {
                throw new RuntimeException("Failed to parse line " + (i + 1) + " of " + journalFile + ": " + line, e);
            }
        }

        return result;
    }

    /**
     * @return Can return <code>null</code>
     */
    public AbstractJournalEntry readJournalLine(String line) throws UnknownEventException {
        if (StringUtils.isEmpty(line)) {
            return null;
        } else {
            LinkedHashMap<String, Object> data = this.gson.fromJson(line, LinkedHashMap.class);
            AbstractJournalEntry entry = this.readJournalData(data);
            if (!data.isEmpty()) {
                logger.debug("Unknown " + entry.getEvent() + " data: " + data);
            }
            return entry;
        }
    }

    /**
     * @return Can return <code>null</code>
     */
    private AbstractJournalEntry readJournalData(LinkedHashMap<String, Object> data) throws UnknownEventException {
        if (data == null || data.isEmpty()) {
            return null;
        } else {
            String timestampValue = (String) data.remove("timestamp");
            Date timestamp = null;
            try {
                timestamp = this.df.parse(timestampValue);
            } catch (ParseException e) {
                throw new RuntimeException("Failed to parse timestamp '" + timestampValue + "'", e);
            }
            String eventName = (String) data.remove("event");
            Event event = null;
            try {
                event = Event.valueOf(eventName);
            } catch (IllegalArgumentException e) {
                throw new UnknownEventException(eventName);
            }

            switch (event) {
                case CrewAssign:
                    return new CrewAssignEntry(timestamp, event, data);
                case Docked:
                    return new DockedEntry(timestamp, event, data);
                case DockingGranted:
                    return new DockingGrantedEntry(timestamp, event, data);
                case DockingRequested:
                    return new DockingRequestedEntry(timestamp, event, data);
                case DockFighter:
                    return new DockFighterEntry(timestamp, event, data);
                case DockSRV:
                    return new DockSRVEntry(timestamp, event, data);
                case EngineerCraft:
                    return new EngineerCraftEntry(timestamp, event, data);
                case Fileheader:
                    return new FileheaderEntry(timestamp, event, data);
                case FSDJump:
                    return new FSDJumpEntry(timestamp, event, data);
                case FuelScoop:
                    return new FuelScoopEntry(timestamp, event, data);
                case HullDamage:
                    return new HullDamageEntry(timestamp, event, data);
                case JetConeBoost:
                    return new JetConeBoostEntry(timestamp, event, data);
                case LaunchFighter:
                    return new LaunchFighterEntry(timestamp, event, data);
                case LaunchSRV:
                    return new LaunchSRVEntry(timestamp, event, data);
                case Liftoff:
                    return new LiftoffEntry(timestamp, event, data);
                case LoadGame:
                    return new LoadGameEntry(timestamp, event, data);
                case Location:
                    return new LocationEntry(timestamp, event, data);
                case MaterialCollected:
                    return new MaterialCollectedEntry(timestamp, event, data);
                case MaterialDiscarded:
                    return new MaterialDiscardedEntry(timestamp, event, data);
                case MaterialDiscovered:
                    return new MaterialDiscoveredEntry(timestamp, event, data);
                case MissionAccepted:
                    return new MissionAcceptedEntry(timestamp, event, data);
                case MissionCompleted:
                    return new MissionCompletedEntry(timestamp, event, data);
                case ModuleBuy:
                    return new ModuleBuyEntry(timestamp, event, data);
                case ModuleRetrieve:
                    return new ModuleRetrieveEntry(timestamp, event, data);
                case ModuleStore:
                    return new ModuleStoreEntry(timestamp, event, data);
                case Progress:
                    return new ProgressEntry(timestamp, event, data);
                case Rank:
                    return new RankEntry(timestamp, event, data);
                case ReceiveText:
                    return new ReceiveTextEntry(timestamp, event, data);
                case RefuelAll:
                    return new RefuelAllEntry(timestamp, event, data);
                case Scan:
                    return new ScanEntry(timestamp, event, data);
                case Screenshot:
                    return new ScreenshotEntry(timestamp, event, data);
                case SellExplorationData:
                    return new SellExplorationDataEntry(timestamp, event, data);
                case SendText:
                    return new SendTextEntry(timestamp, event, data);
                case SupercruiseEntry:
                    return new SupercruiseEntryEntry(timestamp, event, data);
                case SupercruiseExit:
                    return new SupercruiseExitEntry(timestamp, event, data);
                case Touchdown:
                    return new TouchdownEntry(timestamp, event, data);
                default:
                    logger.error("Unhandled event in switch clause: " + event);
                    return null;
            }
        }
    }

}