package borg.edtrading;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.exploration.ScanEntry;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.journal.entries.location.FSDJumpEntry;
import borg.edtrading.journal.entries.location.LocationEntry;
import borg.edtrading.sidepanel.ScannedBody;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ExplorationPayoutCalculator {

    static final Logger logger = LogManager.getLogger(ExplorationPayoutCalculator.class);

    private static final boolean SAFE_CALC = true;

    public static void main(String[] args) throws Exception {
        Journal journal = new Journal(new JournalReader().readEntireJournal(Constants.JOURNAL_DIR));

        String systemName = "Sol";
        SortedMap<String, String> scannedBodyClassesByBodyName = new TreeMap<>();
        SortedMap<String, List<String>> scannedBodyNamesBySystemName = new TreeMap<>();
        for (AbstractJournalEntry entry : journal.getEntries()) {
            if (entry.getEvent() == Event.FSDJump) {
                systemName = ((FSDJumpEntry) entry).getStarSystem();
            } else if (entry.getEvent() == Event.Location) {
                systemName = ((LocationEntry) entry).getStarSystem();
            } else if (entry.getEvent() == Event.Scan) {
                ScanEntry e = (ScanEntry) entry;
                scannedBodyClassesByBodyName.put(e.getBodyName(), new ScannedBody(e).getPayoutKey());
                List<String> list = scannedBodyNamesBySystemName.getOrDefault(systemName, new ArrayList<>());
                list.add(e.getBodyName());
                scannedBodyNamesBySystemName.put(systemName, list);
            }
        }

        LinkedHashMap<String, List<Integer>> payouts = new LinkedHashMap<>();
        List<SellExplorationDataEntry> sellEntries = MiscUtil.unsafeCast(journal.getEntries(null, null, Event.SellExplorationData));
        for (SellExplorationDataEntry entry : sellEntries) {
            if (entry.getSystems().size() == 1) {
                systemName = entry.getSystems().get(0);
                List<String> scannedBodies = scannedBodyNamesBySystemName.getOrDefault(systemName, Collections.emptyList());
                if (scannedBodies.size() <= 1) {
                    if (entry.getDiscovered().size() == 0 && scannedBodies.size() == 0) {
                        // 1 systems w/o scan and consequently also w/o discovery -> plain jump payout
                        int jumpPayout = entry.getBaseValue();

                        List<Integer> payoutsJump = payouts.getOrDefault("JUMP", new ArrayList<>());
                        payoutsJump.add(jumpPayout);
                        payouts.put("JUMP", payoutsJump);
                    } else if (scannedBodies.size() == 1) {
                        // 1 systems w/ 1 scan
                        String date = new SimpleDateFormat("dd. MMM HH:mm").format(entry.getTimestamp());
                        String bodyName = scannedBodies.get(0);
                        String bodyClass = scannedBodyClassesByBodyName.get(bodyName);

                        if (entry.getDiscovered().size() == 1) {
                            int scanPayout = 2 * MiscUtil.getAsInt(entry.getBonus(), 0);
                            int jumpPayout = MiscUtil.getAsInt(entry.getBaseValue(), 0) - scanPayout;

                            List<Integer> payoutsJump = payouts.getOrDefault("JUMP", new ArrayList<>());
                            payoutsJump.add(jumpPayout);
                            payouts.put("JUMP", payoutsJump);

                            List<Integer> payoutsBody = payouts.getOrDefault(bodyClass, new ArrayList<>());
                            payoutsBody.add(scanPayout);
                            payouts.put(bodyClass, payoutsBody);

                            System.out.println(String.format(Locale.US, "[%s] %-35s %,7d + %-,7d    %s (%s)", date, entry.getSystems().get(0) + ":", entry.getBaseValue(), entry.getBonus(), bodyName, bodyClass));
                        } else {
                            if (!SAFE_CALC) {
                                // If we have a good knowledge of the average jump payout we can subtract that
                                // from the base payout. What remains should be relatively close to the payout
                                // for the scanned body.
                                // It is quite unsafe still...
                                List<Integer> payoutsJump = payouts.getOrDefault("JUMP", new ArrayList<>());
                                if (payoutsJump.size() > 100) {
                                    Collections.sort(payoutsJump);
                                    int currentMedianJumpPayout = payoutsJump.get(payoutsJump.size() / 2);
                                    int scanPayout = MiscUtil.getAsInt(entry.getBaseValue(), 0) - currentMedianJumpPayout;
                                    if (scanPayout > 0) {
                                        List<Integer> payoutsBody = payouts.getOrDefault(bodyClass, new ArrayList<>());
                                        payoutsBody.add(scanPayout);
                                        payouts.put(bodyClass, payoutsBody);
                                    }

                                }
                            }
                        }
                    }
                }
            }
        }

        System.out.println();
        //MiscUtil.sortMapByValue(payouts);
        for (String bodyClass : payouts.keySet()) {
            List<Integer> classPayouts = payouts.get(bodyClass);
            Collections.sort(classPayouts);
            int medianPayout = classPayouts.get(classPayouts.size() / 2);

            System.out.println(String.format(Locale.US, "%50s = %-6d %s", bodyClass, medianPayout, classPayouts.toString()));
        }
    }

}
