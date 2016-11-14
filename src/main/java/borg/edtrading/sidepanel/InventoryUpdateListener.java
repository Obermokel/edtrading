package borg.edtrading.sidepanel;

import borg.edtrading.data.Item;
import borg.edtrading.journal.AbstractJournalEntry;
import borg.edtrading.journal.EngineerCraftEntry;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.MaterialCollectedEntry;
import borg.edtrading.journal.MaterialDiscardedEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.LinkedHashMap;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * InventoryUpdateListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryUpdateListener implements JournalUpdateListener {

    static final Logger logger = LogManager.getLogger(InventoryUpdateListener.class);

    private final SortedMap<Item, Integer> inventory;
    private final LinkedHashMap<Item, Integer> income;
    private final LinkedHashMap<Item, Integer> discarded;
    private final LinkedHashMap<Item, Integer> spent;

    public InventoryUpdateListener(Journal journal, String commander) {
        this.inventory = createDefaultInventory(commander);
        this.income = new LinkedHashMap<>();
        this.discarded = new LinkedHashMap<>();
        this.spent = new LinkedHashMap<>();
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
            inventory.put(item, inventory.getOrDefault(item, 0) + e.getCount());
            income.put(item, income.getOrDefault(item, 0) + e.getCount());
        }
    }

    private void onMaterialDiscarded(MaterialDiscardedEntry e) {
        Item item = Item.byJournalName(e.getName());
        if (item == null) {
            logger.warn("Unknown material: " + e.getName() + " (" + e.getCategory() + ")");
        } else {
            inventory.put(item, inventory.getOrDefault(item, 0) - e.getCount());
            discarded.put(item, discarded.getOrDefault(item, 0) + e.getCount());
        }
    }

    private void onEngineerCraft(EngineerCraftEntry e) {
        for (String name : e.getIngredients().keySet()) {
            Item item = Item.byJournalName(name);
            if (item == null) {
                logger.warn("Unknown material: " + name + " (EngineerCraft)");
            } else {
                Integer count = e.getIngredients().get(name);
                inventory.put(item, inventory.getOrDefault(item, 0) - count);
                spent.put(item, spent.getOrDefault(item, 0) + count);
            }
        }
    }

    /**
     * TODO Read from file
     */
    private static SortedMap<Item, Integer> createDefaultInventory(String commander) {
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
            return inventory;
        } else {
            throw new RuntimeException("Unknown commander '" + commander + "'");
        }
    }

}
