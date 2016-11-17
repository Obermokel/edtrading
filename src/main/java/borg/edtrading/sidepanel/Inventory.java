package borg.edtrading.sidepanel;

import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.AbstractJournalEntry;
import borg.edtrading.journal.BuyDronesEntry;
import borg.edtrading.journal.CollectCargoEntry;
import borg.edtrading.journal.EjectCargoEntry;
import borg.edtrading.journal.EngineerCraftEntry;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalReaderThread;
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
public class Inventory implements JournalUpdateListener, GameSessionListener, Serializable {

    private static final long serialVersionUID = 8541359755696166766L;

    static final Logger logger = LogManager.getLogger(Inventory.class);

    private String commander = null;
    private int cargoCapacity = 0;
    private SortedMap<String, Integer> haveByName = new TreeMap<>();
    private SortedMap<String, Integer> collectedByName = new TreeMap<>();
    private SortedMap<String, Integer> discardedByName = new TreeMap<>();
    private SortedMap<String, Integer> spentByName = new TreeMap<>();
    private SortedMap<String, Float> priorityByName = new TreeMap<>();
    private SortedMap<String, Integer> surplusByName = new TreeMap<>();

    private final List<InventoryListener> listeners = new ArrayList<>();

    public Inventory(JournalReaderThread journalReaderThread, GameSession gameSession) {
        if (journalReaderThread != null) {
            journalReaderThread.addListener(this);
        }
        if (gameSession != null) {
            gameSession.addListener(this);
        }
    }

    private void load(String commander) throws IOException {
        for (String name : this.haveByName.keySet()) {
            this.reset(name, 0, guessType(name)); // Reset all existing to 0
        }
        this.collectedByName.clear();
        this.discardedByName.clear();
        this.spentByName.clear();
        this.priorityByName.clear();
        this.surplusByName.clear();

        File file = new File(System.getProperty("user.home"), ".Inventory." + commander + ".json");
        if (!file.exists() || file.length() == 0) {
            // Do nothing
        } else {
            String json = FileUtils.readFileToString(file, "UTF-8");
            LinkedHashMap<String, Map<String, Number>> data = new Gson().fromJson(json, LinkedHashMap.class);
            for (String name : data.get("collected").keySet()) {
                this.collectedByName.put(name, data.get("collected").get(name).intValue());
            }
            for (String name : data.get("discarded").keySet()) {
                this.discardedByName.put(name, data.get("discarded").get(name).intValue());
            }
            for (String name : data.get("spent").keySet()) {
                this.spentByName.put(name, data.get("spent").get(name).intValue());
            }
            for (String name : data.get("have").keySet()) {
                this.reset(name, data.get("have").get(name).intValue(), guessType(name)); // Reset to loaded value
            }
        }
    }

    private void save(String commander) throws IOException {
        if (StringUtils.isNotEmpty(commander)) {
            File file = new File(System.getProperty("user.home"), ".Inventory." + commander + ".json");
            LinkedHashMap<String, SortedMap> data = new LinkedHashMap<>(4);
            data.put("have", this.haveByName);
            data.put("collected", this.collectedByName);
            data.put("discarded", this.discardedByName);
            data.put("spent", this.spentByName);
            String json = new Gson().toJson(data);
            FileUtils.write(file, json, "UTF-8", false);
        }
    }

    public String getCommander() {
        return this.commander;
    }

    public void setCommander(String commander) {
        this.commander = commander;
    }

    public int getCargoCapacity() {
        return this.cargoCapacity;
    }

    public void setCargoCapacity(int cargoCapacity) {
        this.cargoCapacity = cargoCapacity;
    }

    public int getCapacity(ItemType type) {
        if (type == ItemType.DATA) {
            return 500;
        } else if (type == ItemType.ELEMENT || type == ItemType.MANUFACTURED) {
            return 1000;
        } else if (type == ItemType.COMMODITY || type == ItemType.DRONES) {
            return this.getCargoCapacity();
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

    @Override
    public void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship) {
        try {
            this.save(this.getCommander());
        } catch (Exception e) {
            logger.error("Failed to save old inventory for CMDR " + this.getCommander(), e);
        }
        this.setCommander(commander);
        this.setCargoCapacity(ship == null ? 0 : ship.getCargoCapacity());
        try {
            this.load(this.getCommander());
        } catch (Exception e) {
            logger.error("Failed to load new inventory for CMDR " + this.getCommander(), e);
        }
    }

    @Override
    public void onShipModuleChanged(ShipModule oldModule, ShipModule newModule) {
        if (oldModule != null && oldModule.getCargoCapacity() != null) {
            this.setCargoCapacity(this.getCargoCapacity() - oldModule.getCargoCapacity());
        }
        if (newModule != null && newModule.getCargoCapacity() != null) {
            this.setCargoCapacity(this.getCargoCapacity() + newModule.getCargoCapacity());
        }
    }

    @Override
    public void onShipChanged(ShipLoadout oldShip, ShipLoadout newShip) {
        this.setCargoCapacity(newShip == null ? 0 : newShip.getCargoCapacity());
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

}
