package borg.edtrading.sidepanel;

import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.NameCount;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.engineer.EngineerCraftEntry;
import borg.edtrading.journal.entries.game.CargoEntry;
import borg.edtrading.journal.entries.game.MaterialsEntry;
import borg.edtrading.journal.entries.inventory.BuyDronesEntry;
import borg.edtrading.journal.entries.inventory.CollectCargoEntry;
import borg.edtrading.journal.entries.inventory.EjectCargoEntry;
import borg.edtrading.journal.entries.inventory.MaterialCollectedEntry;
import borg.edtrading.journal.entries.inventory.MaterialDiscardedEntry;
import borg.edtrading.journal.entries.inventory.MiningRefinedEntry;
import borg.edtrading.journal.entries.inventory.SellDronesEntry;
import borg.edtrading.journal.entries.inventory.SynthesisEntry;
import borg.edtrading.journal.entries.missions.MissionAcceptedEntry;
import borg.edtrading.journal.entries.missions.MissionCompletedEntry;
import borg.edtrading.journal.entries.starport.MarketBuyEntry;
import borg.edtrading.journal.entries.starport.MarketSellEntry;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
    private SortedMap<String, SortedMap<String, Integer>> haveByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> collectedByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> discardedByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> spentByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Float>> priorityByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> surplusByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> requiredByName = new TreeMap<>();

    private final List<InventoryListener> listeners = new ArrayList<>();

    public Inventory(JournalReaderThread journalReaderThread, GameSession gameSession) {
        if (journalReaderThread != null) {
            journalReaderThread.addListener(this);
        }
        if (gameSession != null) {
            gameSession.addListener(this);
        }
    }

    public Inventory(String commander, Journal journal) throws IOException {
        this.setCommander(commander);
        this.haveByName.put(this.getCommander(), new TreeMap<>());
        this.collectedByName.put(this.getCommander(), new TreeMap<>());
        this.discardedByName.put(this.getCommander(), new TreeMap<>());
        this.spentByName.put(this.getCommander(), new TreeMap<>());
        this.priorityByName.put(this.getCommander(), new TreeMap<>());
        this.surplusByName.put(this.getCommander(), new TreeMap<>());
        this.requiredByName.put(this.getCommander(), new TreeMap<>());
        for (AbstractJournalEntry entry : journal.getEntries()) {
            this.onNewJournalEntry(entry);
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
     * -100% = Useless weight...<br>
     * 0% = Normal stuff<br>
     * +100% = WANT IT!!!
     */
    public float getPriority(String name) {
        return this.priorityByName.get(this.getCommander()).getOrDefault(name, 0f);
    }

    public int getSurplus(String name) {
        return this.surplusByName.get(this.getCommander()).getOrDefault(name, 0);
    }

    public int getRequired(String name) {
        return this.requiredByName.get(this.getCommander()).getOrDefault(name, 0);
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
                    if (e.getName().startsWith("Mission_Delivery")) {
                        // We have been provided with the commodity in order to deliver it somewhere.
                        String journalName = e.getCommodity().replace("$", "").replace("_Name;", "");
                        this.collected(journalName, e.getCount(), ItemType.COMMODITY);
                    } else if (e.getName().startsWith("Mission_Collect")) {
                        // Nothing has happened yet. We have to collect the commodity from somewhere.
                    } else if (e.getName().startsWith("Mission_Passenger") || e.getName().startsWith("Mission_Sightseeing")) {
                        // Ignore passengers asking for cargo
                    } else {
                        logger.warn("Unknown mission accepted type '" + e.getName() + "' which seems to have given us " + e.getCount() + "x " + e.getCommodityLocalized());
                    }
                }
            } else if (entry.getEvent() == Event.MissionCompleted) {
                MissionCompletedEntry e = (MissionCompletedEntry) entry;
                if (e.getCommodityReward() != null) {
                    for (NameCount nc : e.getCommodityReward()) {
                        this.collected(nc.getName(), nc.getCount(), null);
                    }
                }
                if (StringUtils.isNotEmpty(e.getCommodity()) && e.getCount() != null) {
                    if (e.getName().startsWith("Mission_Delivery")) {
                        // We have successfully delivered the commodity which was provided to us.
                        String journalName = e.getCommodity().replace("$", "").replace("_Name;", "");
                        this.discarded(journalName, e.getCount(), ItemType.COMMODITY);
                    } else if (e.getName().startsWith("Mission_Collect")) {
                        // We have successfully collected and delivered the desired commodity.
                        String journalName = e.getCommodity().replace("$", "").replace("_Name;", "");
                        this.discarded(journalName, e.getCount(), ItemType.COMMODITY);
                    } else if (e.getName().startsWith("Mission_Passenger") || e.getName().startsWith("Mission_Sightseeing")) {
                        // Ignore passengers asking for cargo
                    } else {
                        logger.warn("Unknown mission completed type '" + e.getName() + "' which seems to have taken " + e.getCount() + "x " + e.getCommodityLocalized() + " from us");
                    }
                }
            } else if (entry.getEvent() == Event.EngineerCraft) {
                EngineerCraftEntry e = (EngineerCraftEntry) entry;
                if (e.getIngredients() != null) {
                    for (String name : e.getIngredients().keySet()) {
                        this.spent(name, e.getIngredients().get(name), null);
                    }
                }
            } else if (entry.getEvent() == Event.Synthesis) {
                SynthesisEntry e = (SynthesisEntry) entry;
                if (e.getMaterials() != null) {
                    for (String name : e.getMaterials().keySet()) {
                        this.spent(name, e.getMaterials().get(name), null);
                    }
                }
            } else if (entry.getEvent() == Event.ShipyardBuy || entry.getEvent() == Event.ShipyardNew || entry.getEvent() == Event.ShipyardSwap) {
                this.reset(Item.DRONES.getName(), 0, ItemType.DRONES);
            } else if (entry.getEvent() == Event.Died) {
                this.reset(Item.DRONES.getName(), 0, ItemType.DRONES);
                for (String name : this.getNames(ItemType.COMMODITY)) {
                    if (this.getHave(name) > 0) {
                        this.reset(name, 0, ItemType.COMMODITY);
                    }
                }
            } else if (entry.getEvent() == Event.Cargo) {
                CargoEntry e = (CargoEntry) entry;
                for (String name : this.getNames(ItemType.COMMODITY)) {
                    this.reset(name, 0, ItemType.COMMODITY);
                }
                if (e.getInventory() != null) {
                    for (String name : e.getInventory().keySet()) {
                        this.reset(name, e.getInventory().get(name), ItemType.COMMODITY);
                    }
                }
            } else if (entry.getEvent() == Event.Materials) {
                MaterialsEntry e = (MaterialsEntry) entry;
                for (String name : this.getNames(ItemType.ELEMENT)) {
                    this.reset(name, 0, ItemType.ELEMENT);
                }
                if (e.getRaw() != null) {
                    for (String name : e.getRaw().keySet()) {
                        this.reset(name, e.getRaw().get(name), ItemType.ELEMENT);
                    }
                }
                for (String name : this.getNames(ItemType.MANUFACTURED)) {
                    this.reset(name, 0, ItemType.MANUFACTURED);
                }
                if (e.getManufactured() != null) {
                    for (String name : e.getManufactured().keySet()) {
                        this.reset(name, e.getManufactured().get(name), ItemType.MANUFACTURED);
                    }
                }
                for (String name : this.getNames(ItemType.DATA)) {
                    this.reset(name, 0, ItemType.DATA);
                }
                if (e.getEncoded() != null) {
                    for (String name : e.getEncoded().keySet()) {
                        this.reset(name, e.getEncoded().get(name), ItemType.DATA);
                    }
                }
            }
        } catch (Exception e) {
            logger.error("Failed to handle " + entry, e);
        }
    }

    @Override
    public void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship) {
        this.setCommander(commander);
        this.setCargoCapacity(ship == null ? 0 : ship.getCargoCapacity());
        if (this.haveByName.get(this.getCommander()) == null) {
            this.haveByName.put(this.getCommander(), new TreeMap<>());
            this.collectedByName.put(this.getCommander(), new TreeMap<>());
            this.discardedByName.put(this.getCommander(), new TreeMap<>());
            this.spentByName.put(this.getCommander(), new TreeMap<>());
            this.priorityByName.put(this.getCommander(), new TreeMap<>());
            this.surplusByName.put(this.getCommander(), new TreeMap<>());
            this.requiredByName.put(this.getCommander(), new TreeMap<>());
        }
    }

    @Override
    public void onShipModuleChanged(String slot, ShipModule oldModule, ShipModule newModule) {
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

    public synchronized void reset(String name, int count, ItemType type) {
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

    public synchronized void collected(String name, int count, ItemType type) {
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

    public synchronized void discarded(String name, int count, ItemType type) {
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

    public synchronized void spent(String name, int count, ItemType type) {
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
        float normalizedPrio = (prio + 1f) / 2f; // -100% .. +100% -> 0% .. 100%
        int numKeep = 5 + Math.round(20 * normalizedPrio); // 5 .. 25
        float discardPercent = 1f - normalizedPrio; // 0% .. 100% -> 100% .. 0%
        int idealDiscard = Math.round(discardPercent * numHave);
        int maxDiscard = numHave - numKeep;
        int actualDiscard = Math.min(maxDiscard, idealDiscard);
        int surplus = Math.max(0, actualDiscard);
        this.surplusByName.get(this.getCommander()).put(name, surplus);

        // Compute required
        this.requiredByName.get(this.getCommander()).put(name, Math.max(0, numKeep - numHave));
    }

    private static String guessName(String name, ItemType type) {
        Item item = Item.byName(name);

        if (item == null) {
            item = Item.byJournalName(name);
        }

        if (item != null) {
            return item.getName();
        } else {
            logger.warn("Unknown item name '" + name + "'");
            return name.toUpperCase();
        }
    }

    private static ItemType guessType(String name) {
        Item item = Item.byName(name);

        if (item == null) {
            item = Item.byJournalName(name);
        }

        if (item != null) {
            return item.getType();
        } else {
            return ItemType.COMMODITY;
        }
    }

}
