package borg.edtrading.sidepanel;

import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.AbstractJournalEntry;
import borg.edtrading.journal.BuyDronesEntry;
import borg.edtrading.journal.CollectCargoEntry;
import borg.edtrading.journal.EjectCargoEntry;
import borg.edtrading.journal.EngineerCraftEntry;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.MarketBuyEntry;
import borg.edtrading.journal.MarketSellEntry;
import borg.edtrading.journal.MaterialCollectedEntry;
import borg.edtrading.journal.MaterialDiscardedEntry;
import borg.edtrading.journal.MiningRefinedEntry;
import borg.edtrading.journal.MissionAcceptedEntry;
import borg.edtrading.journal.MissionCompletedEntry;
import borg.edtrading.journal.NameCount;
import borg.edtrading.journal.SellDronesEntry;
import com.google.gson.Gson;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Inventory
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Inventory implements JournalUpdateListener, Serializable {

    private static final long serialVersionUID = 8541359755696166766L;

    static final Logger logger = LogManager.getLogger(Inventory.class);

    private final String commander;

    private SortedMap<String, Integer> haveByName = new TreeMap<>();
    private SortedMap<String, Integer> collectedByName = new TreeMap<>();
    private SortedMap<String, Integer> discardedByName = new TreeMap<>();
    private SortedMap<String, Integer> spentByName = new TreeMap<>();
    private SortedMap<String, Float> priorityByName = new TreeMap<>();
    private SortedMap<String, Integer> surplusByName = new TreeMap<>();
    private int cargoCapacity = 192; // TODO Save/load

    private final List<InventoryListener> listeners = new ArrayList<>();

    private Inventory(String commander) {
        this.commander = commander;
    }

    public static Inventory load(String commander) throws IOException {
        Map<String, Number> map = null;
        try {
            File file = new File(System.getProperty("user.home"), ".Inventory." + commander + ".json");
            String json = FileUtils.readFileToString(file, "UTF-8");
            map = new Gson().fromJson(json, LinkedHashMap.class);
        } catch (Exception e) {
            map = createDefaultInventory(commander);
        }

        Inventory inventory = new Inventory(commander);
        for (String name : map.keySet()) {
            inventory.reset(name, map.get(name).intValue(), null);
        }
        return inventory;
    }

    public void save() throws IOException {
        File file = new File(System.getProperty("user.home"), ".Inventory." + this.getCommander() + ".json");
        String json = new Gson().toJson(this.haveByName);
        FileUtils.write(file, json, "UTF-8", false);
    }

    public String getCommander() {
        return this.commander;
    }

    public int getCapacity(ItemType type) {
        if (type == ItemType.DATA) {
            return 500;
        } else if (type == ItemType.ELEMENT || type == ItemType.MANUFACTURED) {
            return 1000;
        } else if (type == ItemType.COMMODITY || type == ItemType.DRONES) {
            return this.cargoCapacity;
        } else {
            return 0;
        }
    }

    public int getTotal(ItemType type) {
        int size = 0;
        for (String name : this.getNames(type)) {
            size += this.getHave(name);
        }
        return size;
    }

    public synchronized List<String> getNames(ItemType type) {
        List<String> names = new ArrayList<>();
        for (String name : this.haveByName.keySet()) {
            ItemType guessedType = guessType(name);
            if (guessedType == type) {
                names.add(name);
            }
        }
        return names;
    }

    public int getHave(String name) {
        return this.haveByName.getOrDefault(name, 0);
    }

    /**
     * -1% = Useless weight...<br>
     * 0% = Normal stuff<br>
     * +1% = WANT IT!!!
     */
    public float getPriority(String name) {
        return this.priorityByName.getOrDefault(name, 0f);
    }

    public int getSurplus(String name) {
        return this.surplusByName.getOrDefault(name, 0);
    }

    public boolean addListener(InventoryListener listener) {
        if (listener == null || this.listeners.contains(listener)) {
            return false;
        } else {
            return this.listeners.add(listener);
        }
    }

    public boolean removeListener(InventoryListener listener) {
        if (listener == null) {
            return false;
        } else {
            return this.listeners.remove(listener);
        }
    }

    @Override
    public void onNewJournalLine(String line) {
        // Do nothing
    }

    @Override
    public void onNewJournalEntry(AbstractJournalEntry entry) {
        try {
            if (entry.getEvent() == Event.MaterialCollected) {
                MaterialCollectedEntry e = (MaterialCollectedEntry) entry;
                this.collected(e.getName(), e.getCount(), null);
            } else if (entry.getEvent() == Event.MaterialDiscarded) {
                MaterialDiscardedEntry e = (MaterialDiscardedEntry) entry;
                this.discarded(e.getName(), e.getCount(), null);
            } else if (entry.getEvent() == Event.CollectCargo) {
                CollectCargoEntry e = (CollectCargoEntry) entry;
                this.collected(e.getType(), 1, ItemType.COMMODITY);
            } else if (entry.getEvent() == Event.EjectCargo) {
                EjectCargoEntry e = (EjectCargoEntry) entry;
                this.discarded(e.getType(), e.getCount(), ItemType.COMMODITY);
            } else if (entry.getEvent() == Event.MarketBuy) {
                MarketBuyEntry e = (MarketBuyEntry) entry;
                this.collected(e.getType(), e.getCount(), ItemType.COMMODITY);
            } else if (entry.getEvent() == Event.MarketSell) {
                MarketSellEntry e = (MarketSellEntry) entry;
                this.spent(e.getType(), e.getCount(), ItemType.COMMODITY);
            } else if (entry.getEvent() == Event.BuyDrones) {
                BuyDronesEntry e = (BuyDronesEntry) entry;
                this.collected(e.getType(), e.getCount(), ItemType.DRONES);
            } else if (entry.getEvent() == Event.SellDrones) {
                SellDronesEntry e = (SellDronesEntry) entry;
                this.discarded(e.getType(), e.getCount(), ItemType.DRONES);
            } else if (entry.getEvent() == Event.MiningRefined) {
                MiningRefinedEntry e = (MiningRefinedEntry) entry;
                this.collected(e.getTypeLocalized(), 1, ItemType.COMMODITY);
            } else if (entry.getEvent() == Event.MissionAccepted) {
                MissionAcceptedEntry e = (MissionAcceptedEntry) entry;
                if (StringUtils.isNotEmpty(e.getCommodity()) && e.getCount() != null) {
                    String journalName = e.getCommodity().replace("$", "").replace("_Name;", "");
                    this.reset(journalName, this.getHave(journalName) + e.getCount(), ItemType.COMMODITY);
                }
            } else if (entry.getEvent() == Event.MissionCompleted) {
                MissionCompletedEntry e = (MissionCompletedEntry) entry;
                if (e.getCommodityReward() != null) {
                    for (NameCount nc : e.getCommodityReward()) {
                        this.collected(nc.getName(), nc.getCount(), null);
                    }
                }
                if (StringUtils.isNotEmpty(e.getCommodity()) && e.getCount() != null) {
                    String journalName = e.getCommodity().replace("$", "").replace("_Name;", "");
                    this.reset(journalName, this.getHave(journalName) - e.getCount(), ItemType.COMMODITY);
                }
            } else if (entry.getEvent() == Event.EngineerCraft) {
                EngineerCraftEntry e = (EngineerCraftEntry) entry;
                if (e.getIngredients() != null) {
                    for (String name : e.getIngredients().keySet()) {
                        this.spent(name, e.getIngredients().get(name), null);
                    }
                }
            } else if (entry.getEvent() == Event.ShipyardBuy || entry.getEvent() == Event.ShipyardNew || entry.getEvent() == Event.ShipyardSwap) {
                this.reset(Item.DRONES.getName(), 0, ItemType.DRONES);
            } else if (entry.getEvent() == Event.Died) {
                this.reset(Item.DRONES.getName(), 0, ItemType.DRONES);
                for (String name : this.getNames(ItemType.COMMODITY)) {
                    this.reset(name, 0, ItemType.COMMODITY);
                }
            }
        } catch (Exception e) {
            logger.error("Failed to handle " + entry, e);
        }
    }

    synchronized void reset(String name, int count, ItemType type) {
        String guessedName = guessName(name, type);

        this.haveByName.put(guessedName, count);

        this.recompute(guessedName);

        for (InventoryListener listener : this.listeners) {
            try {
                listener.onInventoryReset(guessType(guessedName), guessedName, count);
            } catch (Exception e) {
                logger.warn(listener + " failed: " + e);
            }
        }
    }

    synchronized void collected(String name, int count, ItemType type) {
        String guessedName = guessName(name, type);

        this.haveByName.put(guessedName, this.haveByName.getOrDefault(guessedName, 0) + count);
        this.collectedByName.put(guessedName, this.collectedByName.getOrDefault(guessedName, 0) + count);

        this.recompute(guessedName);

        for (InventoryListener listener : this.listeners) {
            try {
                listener.onInventoryCollected(guessType(guessedName), guessedName, count);
            } catch (Exception e) {
                logger.warn(listener + " failed: " + e);
            }
        }
    }

    synchronized void discarded(String name, int count, ItemType type) {
        String guessedName = guessName(name, type);

        this.haveByName.put(guessedName, this.haveByName.getOrDefault(guessedName, 0) - count);
        this.discardedByName.put(guessedName, this.discardedByName.getOrDefault(guessedName, 0) + count);

        this.recompute(guessedName);

        for (InventoryListener listener : this.listeners) {
            try {
                listener.onInventoryDiscarded(guessType(guessedName), guessedName, count);
            } catch (Exception e) {
                logger.warn(listener + " failed: " + e);
            }
        }
    }

    synchronized void spent(String name, int count, ItemType type) {
        String guessedName = guessName(name, type);

        this.haveByName.put(guessedName, this.haveByName.getOrDefault(guessedName, 0) - count);
        this.spentByName.put(guessedName, this.spentByName.getOrDefault(guessedName, 0) + count);

        this.recompute(guessedName);

        for (InventoryListener listener : this.listeners) {
            try {
                listener.onInventorySpent(guessType(guessedName), guessedName, count);
            } catch (Exception e) {
                logger.warn(listener + " failed: " + e);
            }
        }
    }

    private synchronized void recompute(String name) {
        // Compute priority
        float nSpent = this.spentByName.getOrDefault(name, 0);
        float nDiscarded = this.discardedByName.getOrDefault(name, 0);
        float nCollected = this.collectedByName.getOrDefault(name, 0);
        float prio = 0;
        if (nSpent > 0) {
            prio += nSpent / Math.max(nCollected, nSpent); // spent of collected
        }
        if (nDiscarded > 0) {
            prio -= nDiscarded / Math.max(nCollected, nDiscarded); // discarded of collected
        }
        this.priorityByName.put(name, prio);

        // Compute surplus
        int numHave = this.haveByName.getOrDefault(name, 0);
        float normalizedPrio = (prio + 1f) / 2f; // -1% .. +1% -> 0% .. 1%
        int numKeep = 5 + Math.round(20 * normalizedPrio); // 5 .. 25
        float discardPercent = 1f - normalizedPrio; // 0% .. 1% -> 1% .. 0%
        int idealDiscard = Math.round(discardPercent * numHave);
        int maxDiscard = numHave - numKeep;
        int actualDiscard = Math.min(maxDiscard, idealDiscard);
        int surplus = Math.max(0, actualDiscard);
        this.surplusByName.put(name, surplus);
    }

    private static String guessName(String name, ItemType type) {
        Item item = Item.byName(name.toUpperCase());

        if (item == null) {
            item = Item.byJournalName(name.toLowerCase());
        }

        if (item != null) {
            return item.getName();
        } else {
            return name.toUpperCase();
        }
    }

    private static ItemType guessType(String name) {
        Item item = Item.byName(name.toUpperCase());

        if (item == null) {
            item = Item.byJournalName(name.toLowerCase());
        }

        if (item != null) {
            return item.getType();
        } else {
            return ItemType.COMMODITY;
        }
    }

    private static Map<String, Number> createDefaultInventory(String commander) {
        Map<String, Number> inventory = new TreeMap<>();

        if ("Mokel DeLorean".equals(commander)) {
            inventory.put(Item.ANTIMONY.getName(), 5);
            inventory.put(Item.ARSENIC.getName(), 3);
            inventory.put(Item.BASIC_CONDUCTORS.getName(), 10);
            inventory.put(Item.BIOTECH_CONDUCTORS.getName(), 8);
            inventory.put(Item.CADMIUM.getName(), 20);
            inventory.put(Item.CARBON.getName(), 23);
            inventory.put(Item.CHEMICAL_DISTILLERY.getName(), 24);
            inventory.put(Item.CHEMICAL_MANIPULATORS.getName(), 9);
            inventory.put(Item.CHEMICAL_PROCESSORS.getName(), 13);
            inventory.put(Item.CHROMIUM.getName(), 4);
            inventory.put(Item.COMPOUND_SHIELDING.getName(), 19);
            inventory.put(Item.CONDUCTIVE_CERAMICS.getName(), 11);
            inventory.put(Item.CONDUCTIVE_COMPONENTS.getName(), 14);
            inventory.put(Item.CONDUCTIVE_POLYMERS.getName(), 22);
            inventory.put(Item.CONFIGURABLE_COMPONENTS.getName(), 20);
            inventory.put(Item.ELECTROCHEMICAL_ARRAYS.getName(), 11);
            inventory.put(Item.EXQUISITE_FOCUS_CRYSTALS.getName(), 6);
            inventory.put(Item.FLAWED_FOCUS_CRYSTALS.getName(), 4);
            inventory.put(Item.FOCUS_CRYSTALS.getName(), 23);
            inventory.put(Item.GALVANISING_ALLOYS.getName(), 23);
            inventory.put(Item.GERMANIUM.getName(), 20);
            inventory.put(Item.GRID_RESISTORS.getName(), 11);
            inventory.put(Item.HEAT_CONDUCTION_WIRING.getName(), 14);
            inventory.put(Item.HEAT_DISPERSION_PLATE.getName(), 12);
            inventory.put(Item.HEAT_EXCHANGERS.getName(), 13);
            inventory.put(Item.HEAT_VANES.getName(), 20);
            inventory.put(Item.HIGH_DENSITY_COMPOSITES.getName(), 12);
            inventory.put(Item.HYBRID_CAPACITORS.getName(), 10);
            inventory.put(Item.IMPERIAL_SHIELDING.getName(), 3);
            inventory.put(Item.IRON.getName(), 24);
            inventory.put(Item.MANGANESE.getName(), 11);
            inventory.put(Item.MECHANICAL_COMPONENTS.getName(), 12);
            inventory.put(Item.MECHANICAL_EQUIPMENT.getName(), 14);
            inventory.put(Item.MECHANICAL_SCRAP.getName(), 12);
            inventory.put(Item.MERCURY.getName(), 11);
            inventory.put(Item.MILITARY_GRADE_ALLOYS.getName(), 6);
            inventory.put(Item.MILITARY_SUPERCAPACITORS.getName(), 7);
            inventory.put(Item.MOLYBDENUM.getName(), 20);
            inventory.put(Item.NICKEL.getName(), 22);
            inventory.put(Item.NIOBIUM.getName(), 22);
            inventory.put(Item.PHARMACEUTICAL_ISOLATORS.getName(), 4);
            inventory.put(Item.PHASE_ALLOYS.getName(), 11);
            inventory.put(Item.PHOSPHORUS.getName(), 36);
            inventory.put(Item.POLONIUM.getName(), 5);
            inventory.put(Item.POLYMER_CAPACITORS.getName(), 21);
            inventory.put(Item.PRECIPITATED_ALLOYS.getName(), 22);
            inventory.put(Item.PROPRIETARY_COMPOSITES.getName(), 21);
            inventory.put(Item.PROTO_HEAT_RADIATORS.getName(), 3);
            inventory.put(Item.PROTO_LIGHT_ALLOYS.getName(), 28);
            inventory.put(Item.PROTO_RADIOLIC_ALLOYS.getName(), 14);
            inventory.put(Item.REFINED_FOCUS_CRYSTALS.getName(), 27);
            inventory.put(Item.RUTHENIUM.getName(), 6);
            inventory.put(Item.SALVAGED_ALLOYS.getName(), 20);
            inventory.put(Item.SELENIUM.getName(), 21);
            inventory.put(Item.SHIELD_EMITTERS.getName(), 38);
            inventory.put(Item.SHIELDING_SENSORS.getName(), 16);
            inventory.put(Item.SULPHUR.getName(), 32);
            inventory.put(Item.TECHNETIUM.getName(), 1);
            inventory.put(Item.TELLURIUM.getName(), 2);
            inventory.put(Item.THERMIC_ALLOYS.getName(), 18);
            inventory.put(Item.TIN.getName(), 13);
            inventory.put(Item.TUNGSTEN.getName(), 7);
            inventory.put(Item.UNKNOWN_FRAGMENT.getName(), 2);
            inventory.put(Item.VANADIUM.getName(), 21);
            inventory.put(Item.WORN_SHIELD_EMITTERS.getName(), 17);
            inventory.put(Item.YTTRIUM.getName(), 3);
            inventory.put(Item.ZINC.getName(), 21);
            inventory.put(Item.ZIRCONIUM.getName(), 20);

            inventory.put(Item.ABBERANT_SHIELD_PATTERN_ANALYSIS.getName(), 15);
            inventory.put(Item.ABNORMAL_COMPACT_EMISSIONS_DATA.getName(), 10);
            inventory.put(Item.ADAPTIVE_ENCRYPTORS_CAPTURE.getName(), 2);
            inventory.put(Item.ANOMALOUS_BULK_SCAN_DATA.getName(), 20);
            inventory.put(Item.ANOMALOUS_FSD_TELEMETRY.getName(), 21);
            inventory.put(Item.ATYPICAL_DISRUPTED_WAKE_ECHOES.getName(), 11);
            inventory.put(Item.ATYPICAL_ENCRYPTION_ARCHIVES.getName(), 6);
            inventory.put(Item.CLASSIFIED_SCAN_DATABANKS.getName(), 2);
            inventory.put(Item.CLASSIFIED_SCAN_FRAGMENT.getName(), 8);
            inventory.put(Item.CRACKED_INDUSTRIAL_FIRMWARE.getName(), 5);
            inventory.put(Item.DATAMINED_WAKE_EXCEPTIONS.getName(), 4);
            inventory.put(Item.DECODED_EMISSION_DATA.getName(), 13);
            inventory.put(Item.DISTORTED_SHIELD_CYCLE_RECORDINGS.getName(), 29);
            inventory.put(Item.DIVERGENT_SCAN_DATA.getName(), 20);
            inventory.put(Item.ECCENTRIC_HYPERSPACE_TRAJECTORIES.getName(), 18);
            inventory.put(Item.EXCEPTIONAL_SCRAMBLED_EMISSION_DATA.getName(), 12);
            inventory.put(Item.INCONSISTENT_SHIELD_SOAK_ANALYSIS.getName(), 23);
            inventory.put(Item.IRREGULAR_EMISSION_DATA.getName(), 2);
            inventory.put(Item.MODIFIED_CONSUMER_FIRMWARE.getName(), 4);
            inventory.put(Item.OPEN_SYMMETRIC_KEYS.getName(), 5);
            inventory.put(Item.PATTERN_ALPHA_OBELISK_DATA.getName(), 3); // TODO
            inventory.put(Item.PATTERN_BETA_OBELISK_DATA.getName(), 3); // TODO
            inventory.put(Item.PATTERN_GAMMA_OBELISK_DATA.getName(), 3); // TODO
            inventory.put(Item.PATTERN_EPSILON_OBELISK_DATA.getName(), 3); // TODO
            inventory.put(Item.PECULIAR_SHIELD_FREQUENCY_DATA.getName(), 7);
            inventory.put(Item.SECURITY_FIRMWARE_PATCH.getName(), 7);
            inventory.put(Item.SPECIALISED_LEGACY_FIRMWARE.getName(), 11);
            inventory.put(Item.STRANGE_WAKE_SOLUTIONS.getName(), 16);
            inventory.put(Item.TAGGED_ENCRYPTION_CODES.getName(), 8);
            inventory.put(Item.UNEXPECTED_EMISSION_DATA.getName(), 36);
            inventory.put(Item.UNIDENTIFIED_SCAN_ARCHIVES.getName(), 17);
            inventory.put(Item.UNTYPICAL_SHIELD_SCANS.getName(), 25);
            inventory.put(Item.UNUSUAL_ENCRYPTED_FILES.getName(), 6);
        }

        return inventory;
    }

}
