package borg.edtrading;

import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.EngineerCraftEntry;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.journal.MaterialCollectedEntry;
import borg.edtrading.journal.MaterialDiscardedEntry;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * InventoryManagementApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryManagementApp {

    static final Logger logger = LogManager.getLogger(InventoryManagementApp.class);

    public static void main(String[] args) throws Exception {
        final String commander = "Mokel DeLorean";
        final ItemType cleanup = ItemType.MATERIAL;

        Journal journal = new Journal(JournalReader.readEntireJournal(Constants.JOURNAL_DIR));

        Set<String> unknown = new TreeSet<>();
        SortedMap<Item, Integer> inventory = createDefaultInventory(commander);
        LinkedHashMap<Item, Integer> income = new LinkedHashMap<>();
        LinkedHashMap<Item, Integer> discarded = new LinkedHashMap<>();
        LinkedHashMap<Item, Integer> spent = new LinkedHashMap<>();

        List<MaterialCollectedEntry> collectedEntries = MiscUtil.unsafeCast(journal.getEntries(null, null, Event.MaterialCollected));
        for (MaterialCollectedEntry e : collectedEntries) {
            ItemType type = categoryToItemType(e.getCategory());
            Item item = Item.findBestMatching(e.getName(), type);
            if (item == null) {
                unknown.add(e.getName() + " (" + e.getCategory() + ")");
            } else {
                if (cleanup == null || cleanup == type) {
                    inventory.put(item, inventory.getOrDefault(item, 0) + e.getCount());
                    income.put(item, income.getOrDefault(item, 0) + e.getCount());
                }
            }
        }

        List<MaterialDiscardedEntry> discardedEntries = MiscUtil.unsafeCast(journal.getEntries(null, null, Event.MaterialDiscarded));
        for (MaterialDiscardedEntry e : discardedEntries) {
            ItemType type = categoryToItemType(e.getCategory());
            Item item = Item.findBestMatching(e.getName(), type);
            if (item == null) {
                unknown.add(e.getName() + " (" + e.getCategory() + ")");
            } else {
                if (cleanup == null || cleanup == type) {
                    inventory.put(item, inventory.getOrDefault(item, 0) - e.getCount());
                    discarded.put(item, discarded.getOrDefault(item, 0) + e.getCount());
                }
            }
        }

        List<EngineerCraftEntry> craftEntries = MiscUtil.unsafeCast(journal.getEntries(null, null, Event.EngineerCraft));
        for (EngineerCraftEntry e : craftEntries) {
            for (String name : e.getIngredients().keySet()) {
                Item item = Item.findBestMatching(name, null);
                if (item == null) {
                    unknown.add(name + " (Engineer)");
                } else {
                    if (cleanup == null || cleanup == item.getType()) {
                        inventory.put(item, inventory.getOrDefault(item, 0) - e.getIngredients().get(name));
                        spent.put(item, spent.getOrDefault(item, 0) + e.getIngredients().get(name));
                    }
                }
            }
        }

        logger.info("Unknown: " + unknown.size());
        for (String name : unknown) {
            logger.debug(name);
        }

        logger.info("Known: " + inventory.size());
        for (Item i : inventory.keySet()) {
            logger.debug(String.format(Locale.US, "%3dx %s", inventory.get(i), i.getName()));
        }

        // Identify priorities
        LinkedHashMap<Item, Float> priority = new LinkedHashMap<>(); // 1=want have, 0=do not care, -1=hate it
        for (Item item : inventory.keySet()) {
            float nSpent = spent.getOrDefault(item, 0);
            float nDiscarded = discarded.getOrDefault(item, 0);
            float nIncome = income.getOrDefault(item, 0);
            nIncome = Math.max(nIncome, nSpent);
            nIncome = Math.max(nIncome, nDiscarded);
            float spentOfIncome = nSpent / nIncome;
            float discardedOfIncome = nDiscarded / nIncome;
            priority.put(item, spentOfIncome - discardedOfIncome);
        }
        MiscUtil.sortMapByValueReverse(priority);
        logger.info("Prioritized: " + priority.size());
        for (Item i : priority.keySet()) {
            logger.debug(String.format(Locale.US, "%4.0f%%: %s", priority.get(i) * 100f, i.getName()));
        }

        // See what we can discard
        int discardTotal = 0;
        LinkedHashMap<Item, Integer> toDiscard = new LinkedHashMap<>();
        for (Item i : priority.keySet()) {
            Integer numHave = inventory.get(i);
            if (numHave > 1) {
                float discardPercent = ((-1f * priority.get(i)) + 1f) / 2f;
                int numDiscard = Math.round(discardPercent * numHave);
                if (numDiscard >= numHave) {
                    numDiscard = numHave - 1;
                }
                if (numDiscard > 0) {
                    discardTotal += numDiscard;
                    toDiscard.put(i, numDiscard);
                }
            }
        }
        MiscUtil.sortMapByValueReverse(toDiscard);
        logger.info("To discard: " + toDiscard.size());
        for (Item i : toDiscard.keySet()) {
            logger.debug(String.format(Locale.US, "%3dx %s", toDiscard.get(i), i.getName()));
        }
        logger.info("Will free up: " + discardTotal);
    }

    private static ItemType categoryToItemType(String category) {
        if ("Raw".equals(category)) {
            return ItemType.ELEMENT;
        } else if ("Manufactured".equals(category)) {
            return ItemType.MATERIAL;
        } else if ("Encoded".equals(category)) {
            return ItemType.DATA;
        } else if ("Commodity".equals(category)) {
            return ItemType.COMMODITY;
        } else {
            throw new RuntimeException("Unknown category '" + category + "'");
        }
    }

    private static SortedMap<Item, Integer> createDefaultInventory(String commander) {
        if ("Mokel DeLorean".equals(commander)) {
            SortedMap<Item, Integer> inventory = new TreeMap<>();

            return inventory;
        } else {
            throw new RuntimeException("Unknown commander '" + commander + "'");
        }
    }

}
