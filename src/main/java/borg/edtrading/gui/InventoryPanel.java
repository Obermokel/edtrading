package borg.edtrading.gui;

import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.AbstractJournalEntry;
import borg.edtrading.journal.EngineerCraftEntry;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.MaterialCollectedEntry;
import borg.edtrading.journal.MaterialDiscardedEntry;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.Vector;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JTable;

/**
 * InventoryPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryPanel extends JPanel implements JournalUpdateListener {

    private static final long serialVersionUID = 4657223926306497803L;

    static final Logger logger = LogManager.getLogger(InventoryPanel.class);

    private final SortedMap<ItemType, SortedMap<Item, Integer>> inventory;
    private final LinkedHashMap<Item, Integer> income;
    private final LinkedHashMap<Item, Integer> discarded;
    private final LinkedHashMap<Item, Integer> spent;

    private boolean initialized = false;
    private SortedMap<ItemType, LinkedHashMap<Item, Float>> priority = null;
    private SortedMap<ItemType, LinkedHashMap<Item, Integer>> overflow = null;

    public InventoryPanel(Journal journal, String commander) {
        // Set initial state
        this.inventory = createDefaultInventory(commander);
        this.income = new LinkedHashMap<>();
        this.discarded = new LinkedHashMap<>();
        this.spent = new LinkedHashMap<>();

        // Read journal until now
        for (AbstractJournalEntry entry : journal.getEntries()) {
            this.onNewJournalEntry(entry);
        }

        // Ready!
        this.initialized = true;
        this.onInventoryChanged();
    }

    @Override
    public void onNewJournalEntry(AbstractJournalEntry entry) {
        try {
            if (entry.getEvent() == Event.MaterialCollected) {
                this.onMaterialCollected((MaterialCollectedEntry) entry);
            } else if (entry.getEvent() == Event.MaterialDiscarded) {
                this.onMaterialDiscarded((MaterialDiscardedEntry) entry);
            } else if (entry.getEvent() == Event.EngineerCraft) {
                this.onEngineerCraft((EngineerCraftEntry) entry);
            }
        } catch (Exception e) {
            logger.error("Failed to handle " + entry);
        }
    }

    private void onMaterialCollected(MaterialCollectedEntry e) {
        Item item = Item.byJournalName(e.getName());
        if (item == null) {
            logger.warn("Unknown material: " + e.getName() + " (" + e.getCategory() + ")");
        } else {
            SortedMap<Item, Integer> typeInventory = this.inventory.get(item.getType());
            typeInventory.put(item, typeInventory.getOrDefault(item, 0) + e.getCount());
            income.put(item, income.getOrDefault(item, 0) + e.getCount());
            this.onInventoryChanged();
        }
    }

    private void onMaterialDiscarded(MaterialDiscardedEntry e) {
        Item item = Item.byJournalName(e.getName());
        if (item == null) {
            logger.warn("Unknown material: " + e.getName() + " (" + e.getCategory() + ")");
        } else {
            SortedMap<Item, Integer> typeInventory = this.inventory.get(item.getType());
            typeInventory.put(item, typeInventory.getOrDefault(item, 0) - e.getCount());
            discarded.put(item, discarded.getOrDefault(item, 0) + e.getCount());
            this.onInventoryChanged();
        }
    }

    private void onEngineerCraft(EngineerCraftEntry e) {
        for (String name : e.getIngredients().keySet()) {
            Item item = Item.byJournalName(name);
            if (item == null) {
                logger.warn("Unknown material: " + name + " (EngineerCraft)");
            } else {
                Integer count = e.getIngredients().get(name);
                SortedMap<Item, Integer> typeInventory = this.inventory.get(item.getType());
                typeInventory.put(item, typeInventory.getOrDefault(item, 0) - count);
                spent.put(item, spent.getOrDefault(item, 0) + count);
                this.onInventoryChanged();
            }
        }
    }

    private void onInventoryChanged() {
        if (this.initialized) {
            this.recomputePrioritiesAndOverflow();

            this.removeAll();
            this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

            Vector<?> headline = new Vector<>(Arrays.asList("Name", "Have", "Overflow"));

            for (ItemType type : this.inventory.keySet()) {
                Vector<Vector<?>> data = new Vector<>();
                SortedMap<Item, Integer> typeInventory = this.inventory.get(type);
                for (Item item : typeInventory.keySet()) {
                    String name = item.getName();
                    int have = typeInventory.getOrDefault(item, 0);
                    int discard = this.overflow.get(type).getOrDefault(item, 0);
                    data.add(new Vector<>(Arrays.asList(name, have, String.valueOf(discard))));
                }

                // Add the table for this item type
                this.add(new JTable(data, headline));
            }

            this.repaint();
        }
    }

    private void recomputePrioritiesAndOverflow() {
        this.priority = new TreeMap<>();
        this.overflow = new TreeMap<>();

        for (ItemType type : this.inventory.keySet()) {
            SortedMap<Item, Integer> typeInventory = this.inventory.get(type);

            // Identify priorities
            LinkedHashMap<Item, Float> typePriority = new LinkedHashMap<>(); // 1%=want have, 0%=do not care, -1%=hate it
            for (Item item : typeInventory.keySet()) {
                if (item.getType() == type) {
                    float nSpent = spent.getOrDefault(item, 0);
                    float nDiscarded = discarded.getOrDefault(item, 0);
                    float nIncome = income.getOrDefault(item, 0);
                    float prio = 0;
                    if (nSpent > 0) {
                        prio += nSpent / Math.max(nIncome, nSpent); // spent of income
                    }
                    if (nDiscarded > 0) {
                        prio -= nDiscarded / Math.max(nIncome, nDiscarded); // discarded of income
                    }
                    typePriority.put(item, prio);
                }
            }
            MiscUtil.sortMapByValueReverse(typePriority);
            this.priority.put(type, typePriority);
            logger.trace("Prioritized: " + typePriority.size());
            for (Item i : typePriority.keySet()) {
                logger.trace(String.format(Locale.US, "%4.0f%%: %s", typePriority.get(i) * 100f, i.getName()));
            }

            // See what we can discard
            int totalOverflow = 0;
            LinkedHashMap<Item, Integer> typeOverflow = new LinkedHashMap<>();
            for (Item item : typePriority.keySet()) {
                int numHave = typeInventory.get(item);

                float normalizedPrio = (typePriority.get(item) + 1f) / 2f; // -1% .. +1% -> 0% .. 1%
                int numKeep = 5 + Math.round(20 * normalizedPrio); // 5 .. 25

                float discardPercent = 1f - normalizedPrio; // 0% .. 1% -> 1% .. 0%
                int numDiscard = Math.round(discardPercent * numHave);

                int maxDiscard = numHave - numKeep;
                int actualDiscard = Math.min(maxDiscard, numDiscard);

                if (actualDiscard > 0) {
                    totalOverflow += actualDiscard;
                    typeOverflow.put(item, actualDiscard);
                }
            }
            MiscUtil.sortMapByValueReverse(typeOverflow);
            this.overflow.put(type, typeOverflow);
            logger.trace("To discard: " + typeOverflow.size());
            for (Item i : typeOverflow.keySet()) {
                logger.trace(String.format(Locale.US, "%3dx %s", typeOverflow.get(i), i.getName()));
            }
            logger.trace("Will free up: " + totalOverflow);
        }
    }

    /**
     * TODO Read from file
     */
    private static SortedMap<ItemType, SortedMap<Item, Integer>> createDefaultInventory(String commander) {
        if ("Mokel DeLorean".equals(commander)) {
            SortedMap<Item, Integer> inventory = new TreeMap<>();
            inventory.put(Item.ANTIMONY, 5);
            inventory.put(Item.ARSENIC, 3);
            inventory.put(Item.BASIC_CONDUCTORS, 10);
            inventory.put(Item.BIOTECH_CONDUCTORS, 8);
            inventory.put(Item.CADMIUM, 20);
            inventory.put(Item.CARBON, 23);
            inventory.put(Item.CHEMICAL_DISTILLERY, 24);
            inventory.put(Item.CHEMICAL_MANIPULATORS, 9);
            inventory.put(Item.CHEMICAL_PROCESSORS, 13);
            inventory.put(Item.CHROMIUM, 4);
            inventory.put(Item.COMPOUND_SHIELDING, 19);
            inventory.put(Item.CONDUCTIVE_CERAMICS, 11);
            inventory.put(Item.CONDUCTIVE_COMPONENTS, 14);
            inventory.put(Item.CONDUCTIVE_POLYMERS, 22);
            inventory.put(Item.CONFIGURABLE_COMPONENTS, 20);
            inventory.put(Item.ELECTROCHEMICAL_ARRAYS, 11);
            inventory.put(Item.EXQUISITE_FOCUS_CRYSTALS, 6);
            inventory.put(Item.FLAWED_FOCUS_CRYSTALS, 4);
            inventory.put(Item.FOCUS_CRYSTALS, 23);
            inventory.put(Item.GALVANISING_ALLOYS, 23);
            inventory.put(Item.GERMANIUM, 20);
            inventory.put(Item.GRID_RESISTORS, 11);
            inventory.put(Item.HEAT_CONDUCTION_WIRING, 14);
            inventory.put(Item.HEAT_DISPERSION_PLATE, 12);
            inventory.put(Item.HEAT_EXCHANGERS, 13);
            inventory.put(Item.HEAT_VANES, 20);
            inventory.put(Item.HIGH_DENSITY_COMPOSITES, 12);
            inventory.put(Item.HYBRID_CAPACITORS, 10);
            inventory.put(Item.IMPERIAL_SHIELDING, 3);
            inventory.put(Item.IRON, 24);
            inventory.put(Item.MANGANESE, 11);
            inventory.put(Item.MECHANICAL_COMPONENTS, 12);
            inventory.put(Item.MECHANICAL_EQUIPMENT, 14);
            inventory.put(Item.MECHANICAL_SCRAP, 12);
            inventory.put(Item.MERCURY, 11);
            inventory.put(Item.MILITARY_GRADE_ALLOYS, 6);
            inventory.put(Item.MILITARY_SUPERCAPACITORS, 7);
            inventory.put(Item.MOLYBDENUM, 20);
            inventory.put(Item.NICKEL, 22);
            inventory.put(Item.NIOBIUM, 22);
            inventory.put(Item.PHARMACEUTICAL_ISOLATORS, 4);
            inventory.put(Item.PHASE_ALLOYS, 11);
            inventory.put(Item.PHOSPHORUS, 36);
            inventory.put(Item.POLONIUM, 5);
            inventory.put(Item.POLYMER_CAPACITORS, 21);
            inventory.put(Item.PRECIPITATED_ALLOYS, 22);
            inventory.put(Item.PROPRIETARY_COMPOSITES, 21);
            inventory.put(Item.PROTO_HEAT_RADIATORS, 3);
            inventory.put(Item.PROTO_LIGHT_ALLOYS, 28);
            inventory.put(Item.PROTO_RADIOLIC_ALLOYS, 14);
            inventory.put(Item.REFINED_FOCUS_CRYSTALS, 27);
            inventory.put(Item.RUTHENIUM, 6);
            inventory.put(Item.SALVAGED_ALLOYS, 20);
            inventory.put(Item.SELENIUM, 21);
            inventory.put(Item.SHIELD_EMITTERS, 38);
            inventory.put(Item.SHIELDING_SENSORS, 16);
            inventory.put(Item.SULPHUR, 32);
            inventory.put(Item.TECHNETIUM, 1);
            inventory.put(Item.TELLURIUM, 2);
            inventory.put(Item.THERMIC_ALLOYS, 18);
            inventory.put(Item.TIN, 13);
            inventory.put(Item.TUNGSTEN, 7);
            inventory.put(Item.UNKNOWN_FRAGMENT, 2);
            inventory.put(Item.VANADIUM, 21);
            inventory.put(Item.WORN_SHIELD_EMITTERS, 17);
            inventory.put(Item.YTTRIUM, 3);
            inventory.put(Item.ZINC, 21);
            inventory.put(Item.ZIRCONIUM, 20);

            inventory.put(Item.ABBERANT_SHIELD_PATTERN_ANALYSIS, 15);
            inventory.put(Item.ABNORMAL_COMPACT_EMISSIONS_DATA, 10);
            inventory.put(Item.ADAPTIVE_ENCRYPTIONS_CAPTURE, 2);
            inventory.put(Item.ANOMALOUS_BULK_SCAN_DATA, 20);
            inventory.put(Item.ANOMALOUS_FSD_TELEMETRY, 21);
            inventory.put(Item.ATYPICAL_DISRUPTED_WAKE_ECHOES, 11);
            inventory.put(Item.ATYPICAL_ENCRYPTION_ARCHIVES, 6);
            inventory.put(Item.CLASSIFIED_SCAN_DATABANKS, 2);
            inventory.put(Item.CLASSIFIED_SCAN_FRAGMENT, 8);
            inventory.put(Item.CRACKED_INDUSTRIAL_FIRMWARE, 5);
            inventory.put(Item.DATAMINED_WAKE_EXCEPTIONS, 4);
            inventory.put(Item.DECODED_EMISSION_DATA, 13);
            inventory.put(Item.DISTORTED_SHIELD_CYCLE_RECORDINGS, 29);
            inventory.put(Item.DIVERGENT_SCAN_DATA, 20);
            inventory.put(Item.ECCENTRIC_HYPERSPACE_TRAJECTORIES, 18);
            inventory.put(Item.EXCEPTIONAL_SCRAMBLED_EMISSION_DATA, 12);
            inventory.put(Item.INCONSISTENT_SHIELD_SOAK_ANALYSIS, 23);
            inventory.put(Item.IRREGULAR_EMISSION_DATA, 2);
            inventory.put(Item.MODIFIED_CONSUMER_FIRMWARE, 4);
            inventory.put(Item.OPEN_SYMMETRIC_KEYS, 5);
            inventory.put(Item.PATTERN_ALPHA_OBELISK_DATA, 3); // TODO
            inventory.put(Item.PATTERN_BETA_OBELISK_DATA, 3); // TODO
            inventory.put(Item.PATTERN_GAMMA_OBELISK_DATA, 3); // TODO
            inventory.put(Item.PATTERN_EPSILON_OBELISK_DATA, 3); // TODO
            inventory.put(Item.PECULIAR_SHIELD_FREQUENCY_DATA, 7);
            inventory.put(Item.SECURITY_FIRMWARE_PATCH, 7);
            inventory.put(Item.SPECIALISED_LEGACY_FIRMWARE, 11);
            inventory.put(Item.STRANGE_WAKE_SOLUTIONS, 16);
            inventory.put(Item.TAGGED_ENCRYPTION_CODES, 8);
            inventory.put(Item.UNEXPECTED_EMISSION_DATA, 36);
            inventory.put(Item.UNIDENTIFIED_SCAN_ARCHIVES, 17);
            inventory.put(Item.UNTYPICAL_SHIELD_SCANS, 25);
            inventory.put(Item.UNUSUAL_ENCRYPTED_FILES, 6);

            SortedMap<ItemType, SortedMap<Item, Integer>> result = new TreeMap<>();
            result.put(ItemType.ELEMENT, getInventoryOfType(inventory, ItemType.ELEMENT));
            result.put(ItemType.MANUFACTURED, getInventoryOfType(inventory, ItemType.MANUFACTURED));
            result.put(ItemType.DATA, getInventoryOfType(inventory, ItemType.DATA));
            result.put(ItemType.COMMODITY, getInventoryOfType(inventory, ItemType.COMMODITY));
            return result;
        } else {
            throw new RuntimeException("Unknown commander '" + commander + "'");
        }
    }

    private static SortedMap<Item, Integer> getInventoryOfType(Map<Item, Integer> inventory, ItemType type) {
        SortedMap<Item, Integer> result = new TreeMap<>();
        for (Item item : inventory.keySet()) {
            if (item.getType() == type) {
                result.put(item, inventory.get(item));
            }
        }
        return result;
    }

}
