package borg.edtrading;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.exploration.ScanEntry;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.journal.entries.location.FSDJumpEntry;
import borg.edtrading.journal.entries.location.LocationEntry;
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
 * InventoryManagementApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryManagementApp {

    static final Logger logger = LogManager.getLogger(InventoryManagementApp.class);

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
                scannedBodyClassesByBodyName.put(e.getBodyName(), ScanEntry.toBodyClass(e));
                List<String> list = scannedBodyNamesBySystemName.getOrDefault(systemName, new ArrayList<>());
                list.add(e.getBodyName());
                scannedBodyNamesBySystemName.put(systemName, list);
            }
        }

        LinkedHashMap<String, List<Integer>> payouts = new LinkedHashMap<>();
        List<SellExplorationDataEntry> sellEntries = MiscUtil.unsafeCast(journal.getEntries(null, null, Event.SellExplorationData));
        for (SellExplorationDataEntry entry : sellEntries) {
            if (entry.getSystems().size() == 1) {
                List<String> scannedBodies = scannedBodyNamesBySystemName.get(systemName);
                String date = new SimpleDateFormat("dd. MMM HH:mm").format(entry.getTimestamp());
                System.out.println(String.format(Locale.US, "[%s] %-25s %,7d + %,7d    %s", date, entry.getSystems().get(0) + ":", entry.getBaseValue(), entry.getBonus(), entry.getDiscovered().toString()));

                if (entry.getDiscovered().size() == 1) {
                    // 1 systems w/ 1 discovery
                    String bodyName = entry.getDiscovered().get(0);
                    String bodyClass = scannedBodyClassesByBodyName.get(bodyName);
                    int bonus = entry.getBonus();
                    int base = 4 * bonus;
                    int jump = entry.getBaseValue() - base;

                    List<Integer> payoutsBody = payouts.getOrDefault(bodyClass, new ArrayList<>());
                    payoutsBody.add(base);
                    payouts.put(bodyClass, payoutsBody);

                    List<Integer> payoutsJump = payouts.getOrDefault("JUMP", new ArrayList<>());
                    payoutsJump.add(jump);
                    payouts.put("JUMP", payoutsJump);
                } else if (scannedBodies != null && scannedBodies.size() == 1) {
                    // 1 systems w/ 1 scan
                    List<Integer> payoutsJump = payouts.getOrDefault("JUMP", new ArrayList<>());
                    if (payoutsJump.size() > 0) {
                        Collections.sort(payoutsJump);
                        int medianJumpPayout = payoutsJump.get(payoutsJump.size() / 2);

                        String bodyName = scannedBodies.get(0);
                        String bodyClass = scannedBodyClassesByBodyName.get(bodyName);
                        int total = entry.getBaseValue();
                        int base = total - medianJumpPayout;

                        if (base > 0) {
                            List<Integer> payoutsBody = payouts.getOrDefault(bodyClass, new ArrayList<>());
                            payoutsBody.add(base);
                            payouts.put(bodyClass, payoutsBody);
                        }
                    }
                } else if (entry.getDiscovered().size() == 0 && (scannedBodies == null || scannedBodies.size() == 0)) {
                    // 1 systems w/o discovery and scan
                    int jump = entry.getBaseValue();

                    List<Integer> payoutsJump = payouts.getOrDefault("JUMP", new ArrayList<>());
                    payoutsJump.add(jump);
                    payouts.put("JUMP", payoutsJump);
                }
            }
        }

        System.out.println();
        MiscUtil.sortMapByValue(payouts);
        for (String bodyClass : payouts.keySet()) {
            List<Integer> classPayouts = payouts.get(bodyClass);
            Collections.sort(classPayouts);
            int medianPayout = classPayouts.get(classPayouts.size() / 2);

            System.out.println(String.format(Locale.US, "%50s = %-6d %s", bodyClass, medianPayout, classPayouts.toString()));
        }
    }

    //    public static void main(String[] args) throws Exception {
    //        Journal journal = new Journal(new JournalReader().readEntireJournal(Constants.JOURNAL_DIR));
    //
    //        final SortedSet<String> types = new TreeSet<>();
    //        final SortedMap<String, Integer> payouts = new TreeMap<>();
//      //@formatter:off
//        payouts.put(                                  "JUMP", 9000);
//
//        payouts.put(                                     "B", 5000);
//        payouts.put(                                     "A", 4000);
//        payouts.put(                                     "F", 4000);
//        payouts.put(                                     "G", 4000);
//        payouts.put(                                     "K", 3000);
//        payouts.put(                                     "M", 3000);
//        payouts.put(                                     "L", 2500);
//        payouts.put(                                     "Y", 2500);
//
//        payouts.put(                                     "N", 38000);
//        payouts.put(                                    "DA", 20000);
//        payouts.put(                                    "DB", 20000);
//
//        payouts.put(                         "Ammonia world", 50000); // ?
//        payouts.put(                        "Earthlike body", 58000);
//        payouts.put(                           "Water world", 60000);
//        payouts.put( "High metal content body Terraformable", 52000);
//        payouts.put(             "Water world Terraformable", 52000);
//
//        payouts.put(     "Gas giant with ammonia based life", 25000); // ?
//        payouts.put(       "Gas giant with water based life", 25000); // ?
//        payouts.put(            "Sudarsky class I gas giant", 10000); // ?
//        payouts.put(           "Sudarsky class II gas giant", 10000); // ?
//        payouts.put(          "Sudarsky class III gas giant", 10000); // ?
//        payouts.put(           "Sudarsky class IV gas giant", 10000); // ?
//        payouts.put(            "Sudarsky class V gas giant", 10000); // ?
//
//        payouts.put(               "High metal content body", 6000);
//        payouts.put(                              "Icy body", 1100);
//        payouts.put(                       "Metal rich body", 10000);
//        payouts.put(                            "Rocky body", 700);
//        payouts.put(                        "Rocky ice body", 900);
//      //@formatter:on
    //
    //        int nJumpsToUninhabitedSystemsSinceLastSell = 0;
    //        List<String> scannedBodyTypesSinceLastSell = new ArrayList<>();
    //        Integer lastSellAmount = null;
    //        Integer lastSellAmountBonus = null;
    //        Integer estSellAmount = null;
    //        Integer estSellAmountBonus = null;
    //        ListIterator<AbstractJournalEntry> it = journal.getEntries().listIterator();
    //        while (it.hasNext()) {
    //            AbstractJournalEntry entry = it.next();
    //            if (lastSellAmount == null) {
    //                // Search for initial sell
    //                if (entry.getEvent() == Event.SellExplorationData) {
    //                    lastSellAmount = 0;
    //                    lastSellAmountBonus = 0;
    //                    estSellAmount = 0;
    //                    estSellAmountBonus = 0;
    //                }
    //            } else {
    //                if (entry.getEvent() == Event.SellExplorationData) {
    //                    SellExplorationDataEntry e = (SellExplorationDataEntry) entry;
    //                    lastSellAmount = e.getBaseValue();
    //                    lastSellAmountBonus = e.getBonus();
    //                    while (it.hasNext()) {
    //                        entry = it.next();
    //                        if (entry.getEvent() == Event.SellExplorationData) {
    //                            e = (SellExplorationDataEntry) entry;
    //                            lastSellAmount += e.getBaseValue();
    //                            lastSellAmountBonus += e.getBonus();
    //                        } else {
    //                            break;
    //                        }
    //                    }
    //                    if (lastSellAmount >= 1000000) {
    //                        logger.info(new SimpleDateFormat("dd. MMM HH:mm").format(e.getTimestamp()) + ": Sold exploration data for " + String.format(Locale.US, "%,d + %,d = %,d", lastSellAmount, lastSellAmountBonus, lastSellAmount + lastSellAmountBonus)
    //                                + " CR");
    //                        logger.info("Jumps: " + nJumpsToUninhabitedSystemsSinceLastSell);
    //                        logger.info("Scans: " + scannedBodyTypesSinceLastSell.size());
    //                        logger.info(String.format(Locale.US, "%,d est - %,d act = %,d diff", estSellAmount, lastSellAmount, estSellAmount - lastSellAmount));
    //                        logger.debug(new TreeSet<>(scannedBodyTypesSinceLastSell));
    //                        //                    if (lastSellAmount >= 1000000) {
    //                        //                        SortedMap<String, Integer> bestPayouts = new TreeMap<>(payouts);
    //                        //                        for (int i = 0; i < 100; i++) {
    //                        //                            System.out.println("" + i + "...");
    //                        //                            bestPayouts = findBestPayouts(lastSellAmount, nJumpsToUninhabitedSystemsSinceLastSell, scannedBodyTypesSinceLastSell, bestPayouts);
    //                        //                        }
    //                        //                        printPayouts(bestPayouts);
    //                        //                    }
    //                    }
    //                    estSellAmount = 0;
    //                    estSellAmountBonus = 0;
    //                    nJumpsToUninhabitedSystemsSinceLastSell = 0;
    //                    scannedBodyTypesSinceLastSell = new ArrayList<>();
    //                } else if (entry.getEvent() == Event.FSDJump) {
    //                    FSDJumpEntry e = (FSDJumpEntry) entry;
    //                    if ("$government_None;".equals(e.getSystemGovernment())) {
    //                        nJumpsToUninhabitedSystemsSinceLastSell++;
    //                        estSellAmount += payouts.get("JUMP");
    //                    }
    //                } else if (entry.getEvent() == Event.Scan) {
    //                    ScanEntry e = (ScanEntry) entry;
    //                    if (StringUtils.isNotEmpty(e.getPlanetClass())) {
    //                        String planetClass = e.getPlanetClass();
    //                        if (StringUtils.isNotEmpty(e.getTerraformState())) {
    //                            planetClass = planetClass + " " + e.getTerraformState();
    //                        }
    //                        scannedBodyTypesSinceLastSell.add(planetClass);
    //                        estSellAmount += payouts.getOrDefault(planetClass, 0);
    //                        types.add(planetClass);
    //                    } else if (StringUtils.isNotEmpty(e.getStarType())) {
    //                        scannedBodyTypesSinceLastSell.add(e.getStarType());
    //                        estSellAmount += payouts.getOrDefault(e.getStarType(), 0);
    //                        types.add(e.getStarType());
    //                    } else {
    //                        scannedBodyTypesSinceLastSell.add(e.getBodyName());
    //                        estSellAmount += payouts.getOrDefault(e.getBodyName(), 0);
    //                        types.add(e.getBodyName());
    //                    }
    //                }
    //            }
    //        }
    //    }
    //
    //    private static SortedMap<String, Integer> findBestPayouts(int targetAmount, int nJumps, List<String> scanned, SortedMap<String, Integer> startPayouts) {
    //        SortedMap<String, Integer> bestPayouts = null;
    //        int bestDiff = 999999999;
    //
    //        for (String type : startPayouts.keySet()) {
    //            for (int amount = 100; amount <= 75000; amount += 100) {
    //                SortedMap<String, Integer> payouts = new TreeMap<>(startPayouts);
    //                payouts.put(type, amount);
    //                int estimatedAmount = estimate(nJumps, scanned, payouts);
    //                int diff = Math.abs(targetAmount - estimatedAmount);
    //                if (bestPayouts == null || diff < bestDiff) {
    //                    bestPayouts = payouts;
    //                    bestDiff = diff;
    //                }
    //            }
    //        }
    //
    //        return bestPayouts;
    //    }
    //
    //    private static int estimate(int nJumps, List<String> scanned, SortedMap<String, Integer> payouts) {
    //        int estimatedAmount = nJumps * payouts.get("JUMP");
    //        for (String type : scanned) {
    //            estimatedAmount += payouts.get(type);
    //        }
    //        return estimatedAmount;
    //    }
    //
    //    private static void printPayouts(SortedMap<String, Integer> payouts) {
    //        for (String type : payouts.keySet()) {
    //            System.out.println(String.format(Locale.US, "payouts.put(%40s, %d);", "\"" + type + "\"", payouts.get(type)));
    //        }
    //    }

}
