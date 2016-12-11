package borg.edtrading.sidepanel;

import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.NameCount;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.engineer.EngineerCraftEntry;
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
public class Inventory implements GameSessionListener, Serializable {

    private static final long serialVersionUID = 8541359755696166766L;

    static final Logger logger = LogManager.getLogger(Inventory.class);

    private String commander = null;
    private int cargoCapacity = 0;

    private final List<InventoryListener> listeners = new ArrayList<>();

    public Inventory(JournalReaderThread journalReaderThread, GameSession gameSession) {
        if (gameSession != null) {
            gameSession.addListener(this);
        }
    }

    public Inventory(String commander, Journal journal) throws IOException {
        this.setCommander(commander);
        this.offsetByName.put(this.getCommander(), new TreeMap<>());
        this.haveByName.put(this.getCommander(), new TreeMap<>());
        this.collectedByName.put(this.getCommander(), new TreeMap<>());
        this.discardedByName.put(this.getCommander(), new TreeMap<>());
        this.spentByName.put(this.getCommander(), new TreeMap<>());
        this.priorityByName.put(this.getCommander(), new TreeMap<>());
        this.surplusByName.put(this.getCommander(), new TreeMap<>());
        this.requiredByName.put(this.getCommander(), new TreeMap<>());
        this.loadOffsets(this.getCommander());
        for (AbstractJournalEntry entry : journal.getEntries()) {
            this.onNewJournalEntry(entry);
        }
    }

    private void loadOffsets(String commander) throws IOException {
        File dir = new File(System.getProperty("user.home"), ".edsidepanel");
        if (!dir.exists()) {
            dir.mkdirs();
        }
        File file = new File(dir, "InventoryOffsets." + commander + ".json");
        if (!file.exists() || file.length() == 0) {
            // Do nothing
        } else {
            String json = FileUtils.readFileToString(file, "UTF-8");
            TreeMap<String, Number> offsets = new Gson().fromJson(json, TreeMap.class);
            for (String name : offsets.keySet()) {
                String guessedName = guessName(name, null);
                try {
                    this.offsetByName.get(commander).put(guessedName, offsets.get(name).intValue());
                    this.haveByName.get(commander).put(guessedName, offsets.get(name).intValue());
                } catch (Exception e) {
                    logger.error("Failed to load offset for " + name + " (" + guessedName + ")", e);
                }
            }
        }
    }

    private void saveOffsets(String commander) throws IOException {
        if (StringUtils.isNotEmpty(commander)) {
            File dir = new File(System.getProperty("user.home"), ".edsidepanel");
            if (!dir.exists()) {
                dir.mkdirs();
            }
            File file = new File(dir, "InventoryOffsets." + commander + ".json");
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
            File dir = new File(System.getProperty("user.home"), ".edsidepanel");
            if (!dir.exists()) {
                dir.mkdirs();
            }
            File file = new File(dir, "Inventory." + this.getCommander() + ".json");
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

    public void changeOffset(String name, int offsetChange) {
        int prev = this.offsetByName.get(this.getCommander()).getOrDefault(name, 0);
        this.offsetByName.get(this.getCommander()).put(name, prev + offsetChange);
        prev = this.haveByName.get(this.getCommander()).getOrDefault(name, 0);
        this.haveByName.get(this.getCommander()).put(name, prev + offsetChange);
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

    private void onNewJournalEntry(AbstractJournalEntry entry) {
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
                this.requiredByName.put(this.getCommander(), new TreeMap<>());
                this.loadOffsets(this.getCommander());
            }
        } catch (Exception e) {
            logger.error("Failed to load offsets for CMDR " + this.getCommander(), e);
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

}
