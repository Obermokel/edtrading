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
import java.util.List;
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
    private SortedMap<String, SortedMap<String, Integer>> offsetByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> haveByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> collectedByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> discardedByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> spentByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Float>> priorityByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> surplusByName = new TreeMap<>();

    private final List<InventoryListener> listeners = new ArrayList<>();

    public Inventory(JournalReaderThread journalReaderThread, GameSession gameSession) {
        if (journalReaderThread != null) {
            journalReaderThread.addListener(this);
        }
        if (gameSession != null) {
            gameSession.addListener(this);
        }
    }

    private void loadOffsets(String commander) throws IOException {
        File file = new File(System.getProperty("user.home"), ".InventoryOffsets." + commander + ".json");
        if (!file.exists() || file.length() == 0) {
            // Do nothing
        } else {
            String json = FileUtils.readFileToString(file, "UTF-8");
            TreeMap<String, Number> offsets = new Gson().fromJson(json, TreeMap.class);
            for (String name : offsets.keySet()) {
                this.offsetByName.get(commander).put(name, offsets.get(name).intValue());
                this.haveByName.get(commander).put(name, offsets.get(name).intValue());
            }
        }
    }

    private void saveOffsets(String commander) throws IOException {
        if (StringUtils.isNotEmpty(commander)) {
            File file = new File(System.getProperty("user.home"), ".InventoryOffsets." + commander + ".json");
            SortedMap<String, Integer> offsets = new TreeMap<>();
            for (String name : this.haveByName.get(commander).keySet()) {
                offsets.put(name, this.offsetByName.get(commander).getOrDefault(name, 0));
            }
            String json = new Gson().toJson(offsets);
            FileUtils.write(file, json, "UTF-8", false);
        }
    }

    public void save() throws IOException {
        if (StringUtils.isNotEmpty(this.getCommander())) {
            File file = new File(System.getProperty("user.home"), ".Inventory." + this.getCommander() + ".json");
            String json = new Gson().toJson(this.haveByName.get(this.getCommander()));
            FileUtils.write(file, json, "UTF-8", false);

            this.saveOffsets(this.getCommander());
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
        for (String name : this.haveByName.get(this.getCommander()).keySet()) {
            ItemType guessedType = guessType(name);
            if (guessedType == type) {
                names.add(name);
            }
        }
        return names;
    }

    public int getHave(String name) {
        return this.haveByName.get(this.getCommander()).getOrDefault(name, 0);
    }

    /**
     * -1% = Useless weight...<br>
     * 0% = Normal stuff<br>
     * +1% = WANT IT!!!
     */
    public float getPriority(String name) {
        return this.priorityByName.get(this.getCommander()).getOrDefault(name, 0f);
    }

    public int getSurplus(String name) {
        return this.surplusByName.get(this.getCommander()).getOrDefault(name, 0);
    }

    public void incOffset(String name) {
        int prev = this.offsetByName.get(this.getCommander()).getOrDefault(name, 0);
        this.offsetByName.get(this.getCommander()).put(name, prev + 1);
        prev = this.haveByName.get(this.getCommander()).getOrDefault(name, 0);
        this.haveByName.get(this.getCommander()).put(name, prev + 1);
    }

    public void decOffset(String name) {
        int prev = this.offsetByName.get(this.getCommander()).getOrDefault(name, 0);
        this.offsetByName.get(this.getCommander()).put(name, prev - 1);
        prev = this.haveByName.get(this.getCommander()).getOrDefault(name, 0);
        this.haveByName.get(this.getCommander()).put(name, prev - 1);
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
                    this.collected(journalName, e.getCount(), ItemType.COMMODITY);
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
                    this.discarded(journalName, e.getCount(), ItemType.COMMODITY);
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
            this.save();
        } catch (Exception e) {
            logger.error("Failed to save old inventory for CMDR " + this.getCommander(), e);
        }
        this.setCommander(commander);
        this.setCargoCapacity(ship == null ? 0 : ship.getCargoCapacity());
        try {
            if (this.offsetByName.get(this.getCommander()) == null) {
                this.offsetByName.put(this.getCommander(), new TreeMap<>());
                this.haveByName.put(this.getCommander(), new TreeMap<>());
                this.collectedByName.put(this.getCommander(), new TreeMap<>());
                this.discardedByName.put(this.getCommander(), new TreeMap<>());
                this.spentByName.put(this.getCommander(), new TreeMap<>());
                this.priorityByName.put(this.getCommander(), new TreeMap<>());
                this.surplusByName.put(this.getCommander(), new TreeMap<>());
                this.loadOffsets(this.getCommander());
            }
        } catch (Exception e) {
            logger.error("Failed to load offsets for CMDR " + this.getCommander(), e);
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

        this.haveByName.get(this.getCommander()).put(guessedName, count);

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

        this.haveByName.get(this.getCommander()).put(guessedName, this.haveByName.get(this.getCommander()).getOrDefault(guessedName, 0) + count);
        this.collectedByName.get(this.getCommander()).put(guessedName, this.collectedByName.get(this.getCommander()).getOrDefault(guessedName, 0) + count);

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

        this.haveByName.get(this.getCommander()).put(guessedName, this.haveByName.get(this.getCommander()).getOrDefault(guessedName, 0) - count);
        this.discardedByName.get(this.getCommander()).put(guessedName, this.discardedByName.get(this.getCommander()).getOrDefault(guessedName, 0) + count);

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

        this.haveByName.get(this.getCommander()).put(guessedName, this.haveByName.get(this.getCommander()).getOrDefault(guessedName, 0) - count);
        this.spentByName.get(this.getCommander()).put(guessedName, this.spentByName.get(this.getCommander()).getOrDefault(guessedName, 0) + count);

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
        float nSpent = this.spentByName.get(this.getCommander()).getOrDefault(name, 0);
        float nDiscarded = this.discardedByName.get(this.getCommander()).getOrDefault(name, 0);
        float nCollected = this.collectedByName.get(this.getCommander()).getOrDefault(name, 0);
        float prio = 0;
        if (nSpent > 0) {
            prio += nSpent / Math.max(nCollected, nSpent); // spent of collected
        }
        if (nDiscarded > 0) {
            prio -= nDiscarded / Math.max(nCollected, nDiscarded); // discarded of collected
        }
        this.priorityByName.get(this.getCommander()).put(name, prio);

        // Compute surplus
        int numHave = this.haveByName.get(this.getCommander()).getOrDefault(name, 0);
        float normalizedPrio = (prio + 1f) / 2f; // -1% .. +1% -> 0% .. 1%
        int numKeep = 5 + Math.round(20 * normalizedPrio); // 5 .. 25
        float discardPercent = 1f - normalizedPrio; // 0% .. 1% -> 1% .. 0%
        int idealDiscard = Math.round(discardPercent * numHave);
        int maxDiscard = numHave - numKeep;
        int actualDiscard = Math.min(maxDiscard, idealDiscard);
        int surplus = Math.max(0, actualDiscard);
        this.surplusByName.get(this.getCommander()).put(name, surplus);
    }

    private static String guessName(String name, ItemType type) {
        Item item = Item.byName(name.toUpperCase());

        if (item == null) {
            item = Item.byJournalName(name.toLowerCase());
        }

        if (item != null) {
            return item.getName();
        } else {
            logger.warn("Unknown item name '" + name + "'");
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
