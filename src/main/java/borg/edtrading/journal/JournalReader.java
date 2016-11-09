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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

/**
 * JournalReader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class JournalReader {

    static final Logger logger = LogManager.getLogger(JournalReader.class);

    private static final LinkedHashMap<String, Integer> UNKNOWN_EVENTS = new LinkedHashMap<>();

    public static List<AbstractJournalEntry> readEntireJournal(File journalDir) throws IOException {
        List<AbstractJournalEntry> result = new ArrayList<>();

        UNKNOWN_EVENTS.clear();

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
        for (File journalFile : journalFiles) {
            result.addAll(JournalReader.readJournalFile(journalFile));
        }

        MiscUtil.sortMapByValue(UNKNOWN_EVENTS);
        for (String eventName : UNKNOWN_EVENTS.keySet()) {
            logger.debug(String.format(Locale.US, "Unknown event: %4dx %s", UNKNOWN_EVENTS.get(eventName), eventName));
        }

        return result;
    }

    public static List<AbstractJournalEntry> readJournalFile(File journalFile) throws IOException {
        List<AbstractJournalEntry> result = new ArrayList<>();

        final DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
        final Gson gson = new Gson();
        List<String> lines = FileUtils.readLines(journalFile, "UTF-8");
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            if (StringUtils.isNotEmpty(line)) {
                try {
                    LinkedHashMap<String, Object> data = gson.fromJson(line, LinkedHashMap.class);
                    Date timestamp = df.parse((String) data.remove("timestamp"));
                    String eventName = (String) data.remove("event");
                    Event event = null;
                    try {
                        event = Event.valueOf(eventName);
                    } catch (IllegalArgumentException e) {
                        logger.trace("Unknown event type '" + eventName + "' in line " + (i + 1) + " of " + journalFile);
                        Integer count = UNKNOWN_EVENTS.get(eventName);
                        if (count == null) {
                            count = new Integer(0);
                            UNKNOWN_EVENTS.put(eventName, count);
                        }
                        UNKNOWN_EVENTS.put(eventName, UNKNOWN_EVENTS.get(eventName) + 1);
                    }
                    if (event != null) {
                        switch (event) {
                            case CrewAssign:
                                result.add(new CrewAssignEntry(timestamp, event, data));
                                break;
                            case Docked:
                                result.add(new DockedEntry(timestamp, event, data));
                                break;
                            case DockingGranted:
                                result.add(new DockingGrantedEntry(timestamp, event, data));
                                break;
                            case DockingRequested:
                                result.add(new DockingRequestedEntry(timestamp, event, data));
                                break;
                            case DockFighter:
                                result.add(new DockFighterEntry(timestamp, event, data));
                                break;
                            case DockSRV:
                                result.add(new DockSRVEntry(timestamp, event, data));
                                break;
                            case Fileheader:
                                result.add(new FileheaderEntry(timestamp, event, data));
                                break;
                            case FSDJump:
                                result.add(new FSDJumpEntry(timestamp, event, data));
                                break;
                            case FuelScoop:
                                result.add(new FuelScoopEntry(timestamp, event, data));
                                break;
                            case HullDamage:
                                result.add(new HullDamageEntry(timestamp, event, data));
                                break;
                            case JetConeBoost:
                                result.add(new JetConeBoostEntry(timestamp, event, data));
                                break;
                            case LaunchFighter:
                                result.add(new LaunchFighterEntry(timestamp, event, data));
                                break;
                            case LaunchSRV:
                                result.add(new LaunchSRVEntry(timestamp, event, data));
                                break;
                            case Liftoff:
                                result.add(new LiftoffEntry(timestamp, event, data));
                                break;
                            case LoadGame:
                                result.add(new LoadGameEntry(timestamp, event, data));
                                break;
                            case Location:
                                result.add(new LocationEntry(timestamp, event, data));
                                break;
                            case MaterialCollected:
                                result.add(new MaterialCollectedEntry(timestamp, event, data));
                                break;
                            case ModuleBuy:
                                result.add(new ModuleBuyEntry(timestamp, event, data));
                                break;
                            case ModuleRetrieve:
                                result.add(new ModuleRetrieveEntry(timestamp, event, data));
                                break;
                            case ModuleStore:
                                result.add(new ModuleStoreEntry(timestamp, event, data));
                                break;
                            case Progress:
                                result.add(new ProgressEntry(timestamp, event, data));
                                break;
                            case Rank:
                                result.add(new RankEntry(timestamp, event, data));
                                break;
                            case ReceiveText:
                                result.add(new ReceiveTextEntry(timestamp, event, data));
                                break;
                            case RefuelAll:
                                result.add(new RefuelAllEntry(timestamp, event, data));
                                break;
                            case Scan:
                                result.add(new ScanEntry(timestamp, event, data));
                                break;
                            case Screenshot:
                                result.add(new ScreenshotEntry(timestamp, event, data));
                                break;
                            case SellExplorationData:
                                result.add(new SellExplorationDataEntry(timestamp, event, data));
                                break;
                            case SendText:
                                result.add(new SendTextEntry(timestamp, event, data));
                                break;
                            case SupercruiseEntry:
                                result.add(new SupercruiseEntryEntry(timestamp, event, data));
                                break;
                            case SupercruiseExit:
                                result.add(new SupercruiseExitEntry(timestamp, event, data));
                                break;
                            case Touchdown:
                                result.add(new TouchdownEntry(timestamp, event, data));
                                break;
                            default:
                                break;
                        }

                        if (!data.isEmpty()) {
                            logger.warn("Unknown " + event + " data: " + data);
                        }
                    }
                } catch (Exception e) {
                    throw new RuntimeException("Failed to parse line " + (i + 1) + " of " + journalFile + ": " + line, e);
                }
            }
        }

        return result;
    }

}
