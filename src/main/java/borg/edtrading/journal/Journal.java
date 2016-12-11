package borg.edtrading.journal;

import borg.edtrading.data.Coord;
import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.exploration.ScanEntry;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.journal.entries.fleet.FetchRemoteModuleEntry;
import borg.edtrading.journal.entries.fleet.ModuleBuyEntry;
import borg.edtrading.journal.entries.fleet.ModuleRetrieveEntry;
import borg.edtrading.journal.entries.fleet.ModuleSellEntry;
import borg.edtrading.journal.entries.fleet.ModuleSellRemoteEntry;
import borg.edtrading.journal.entries.fleet.ModuleStoreEntry;
import borg.edtrading.journal.entries.fleet.ModuleSwapEntry;
import borg.edtrading.journal.entries.fleet.ShipyardBuyEntry;
import borg.edtrading.journal.entries.fleet.ShipyardNewEntry;
import borg.edtrading.journal.entries.fleet.ShipyardSwapEntry;
import borg.edtrading.journal.entries.game.LoadGameEntry;
import borg.edtrading.journal.entries.location.DockedEntry;
import borg.edtrading.journal.entries.location.FSDJumpEntry;
import borg.edtrading.journal.entries.location.LiftoffEntry;
import borg.edtrading.journal.entries.location.LocationEntry;
import borg.edtrading.journal.entries.location.SupercruiseEntryEntry;
import borg.edtrading.journal.entries.location.SupercruiseExitEntry;
import borg.edtrading.journal.entries.location.TouchdownEntry;
import borg.edtrading.journal.entries.location.UndockedEntry;
import borg.edtrading.journal.entries.starport.RefuelAllEntry;
import borg.edtrading.journal.entries.travel.FuelScoopEntry;
import borg.edtrading.journal.entries.travel.JetConeBoostEntry;
import borg.edtrading.sidepanel.ScannedBody;
import borg.edtrading.sidepanel.ShipLoadout;
import borg.edtrading.sidepanel.ShipModule;
import borg.edtrading.sidepanel.StoredModules;
import borg.edtrading.sidepanel.VisitedSystem;
import borg.edtrading.util.MiscUtil;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Journal
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Journal {

    static final Logger logger = LogManager.getLogger(Journal.class);

    private final List<JournalListener> listeners = new ArrayList<>();

    private final LinkedList<AbstractJournalEntry> entries = new LinkedList<>(); // TODO Reverse order so that first list element is latest journal entry
    private final LinkedList<VisitedSystem> visitedSystems = new LinkedList<>(); // TODO Reverse order so that first list element is last visited system

    private Coord coord = null;
    private String systemName = null;
    private String bodyName = null;
    private String bodyType = null;
    private String stationName = null;
    private String stationType = null;
    private Float latitude = null;
    private Float longitude = null;
    private String faction = null;
    private String allegiance = null;
    private String economy = null;
    private String state = null;
    private String government = null;
    private String security = null;
    private boolean inSupercruise = false;
    private boolean landed = true;
    private float fuelLevel = 0;
    private float boostLevel = 1;

    private String commander = null;
    private String gameMode = null;
    private String group = null;
    private Integer currentShipID = null;
    private String currentShipType = null;
    private ShipLoadout currentShipLoadout = null;
    private StoredModules storedModules = null;
    private String lastStation = null; // TODO required or travelhistory?

    private SortedMap<String, SortedMap<String, Integer>> offsetByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> haveByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> collectedByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> discardedByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> spentByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Float>> priorityByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> surplusByName = new TreeMap<>();
    private SortedMap<String, SortedMap<String, Integer>> requiredByName = new TreeMap<>();

    public Journal() {
        this.setCommander("Mokel DeLorean [GPL]"); // TODO
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

    public boolean addListener(JournalListener listener) {
        if (listener == null || this.listeners.contains(listener)) {
            return false;
        } else {
            return this.listeners.add(listener);
        }
    }

    public boolean removeListener(JournalListener listener) {
        if (listener == null) {
            return false;
        } else {
            return this.listeners.remove(listener);
        }
    }

    public ShipLoadout loadShipLoadout(String commander, Integer shipID, String shipType) {
        try {
            File dir = new File(System.getProperty("user.home"), ".edsidepanel");
            File file = new File(dir, "ShipLoadout." + commander + "." + shipID + "_" + shipType + ".json");
            if (file.exists() && file.length() > 0) {
                String json = FileUtils.readFileToString(file, "UTF-8");
                return new Gson().fromJson(json, ShipLoadout.class);
            }
        } catch (JsonSyntaxException | IOException e) {
            logger.error("Failed to load ship loadout #" + shipID + " '" + shipType + "' for commander " + commander, e);
        }
        return new ShipLoadout(shipID, shipType);
    }

    public void saveShipLoadout(String commander, ShipLoadout shipLoadout) {
        if (shipLoadout != null) {
            try {
                File dir = new File(System.getProperty("user.home"), ".edsidepanel");
                if (!dir.exists()) {
                    dir.mkdirs();
                }
                File file = new File(dir, "ShipLoadout." + commander + "." + shipLoadout.getShipID() + "_" + shipLoadout.getShipType() + ".json");
                String json = new Gson().toJson(shipLoadout);
                FileUtils.write(file, json, "UTF-8", false);
            } catch (IOException e) {
                logger.error("Failed to save ship loadout '" + shipLoadout + "' for commander " + commander, e);
            }
        }
    }

    public StoredModules loadStoredModules(String commander) {
        try {
            File dir = new File(System.getProperty("user.home"), ".edsidepanel");
            File file = new File(dir, "StoredModules." + commander + ".json");
            if (file.exists() && file.length() > 0) {
                String json = FileUtils.readFileToString(file, "UTF-8");
                return new Gson().fromJson(json, StoredModules.class);
            }
        } catch (JsonSyntaxException | IOException e) {
            logger.error("Failed to load stored modules for commander " + commander, e);
        }
        return new StoredModules();
    }

    public void saveStoredModules(String commander, StoredModules storedModules) {
        if (storedModules != null) {
            try {
                File dir = new File(System.getProperty("user.home"), ".edsidepanel");
                if (!dir.exists()) {
                    dir.mkdirs();
                }
                File file = new File(dir, "StoredModules." + commander + ".json");
                String json = new Gson().toJson(storedModules);
                FileUtils.write(file, json, "UTF-8", false);
            } catch (IOException e) {
                logger.error("Failed to save stored modules '" + storedModules + "' for commander " + commander, e);
            }
        }
    }

    public String loadLastStation(String commander) {
        try {
            File dir = new File(System.getProperty("user.home"), ".edsidepanel");
            File file = new File(dir, "LastStation." + commander + ".txt");
            if (file.exists() && file.length() > 0) {
                return FileUtils.readFileToString(file, "UTF-8");
            }
        } catch (IOException e) {
            logger.error("Failed to load last station for commander " + commander, e);
        }
        return null;
    }

    public void saveLastStation(String commander, String stationName) {
        if (stationName != null) {
            try {
                File dir = new File(System.getProperty("user.home"), ".edsidepanel");
                if (!dir.exists()) {
                    dir.mkdirs();
                }
                File file = new File(dir, "LastStation." + commander + ".txt");
                FileUtils.write(file, stationName, "UTF-8", false);
            } catch (IOException e) {
                logger.error("Failed to save last station '" + lastStation + "' for commander " + commander, e);
            }
        }
    }

    public void loadOffsets(String commander) {
        try {
            File dir = new File(System.getProperty("user.home"), ".edsidepanel");
            File file = new File(dir, "InventoryOffsets." + commander + ".json");
            if (file.exists() && file.length() > 0) {
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
        } catch (IOException e) {
            logger.error("Failed to load inventory offsets for commander " + commander, e);
        }
    }

    public void saveOffsets(String commander) {
        if (StringUtils.isNotEmpty(commander)) {
            try {
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
            } catch (IOException e) {
                logger.error("Failed to save inventory offsets for commander " + commander, e);
            }
        }
    }

    public synchronized void reset(String name, int count, ItemType type) {
        String guessedName = guessName(name, type);

        this.haveByName.get(this.getCommander()).put(guessedName, count);

        this.recompute(guessedName);

        for (JournalListener listener : this.listeners) {
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

        for (JournalListener listener : this.listeners) {
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

        for (JournalListener listener : this.listeners) {
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

        for (JournalListener listener : this.listeners) {
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

    /**
     * The caller has to take care that the entry timestamp is &gt;= the last timestamp!
     */
    void add(AbstractJournalEntry entry) {
        if (entry != null) {
            // Add
            this.entries.add(entry);

            // Process
            try {
                if (entry.getEvent() == Event.Location) {
                    this.handleLocationEntry((LocationEntry) entry);
                } else if (entry.getEvent() == Event.FSDJump) {
                    this.handleFSDJumpEntry((FSDJumpEntry) entry);
                } else if (entry.getEvent() == Event.FuelScoop) {
                    this.handleFuelScoopEntry((FuelScoopEntry) entry);
                } else if (entry.getEvent() == Event.JetConeBoost) {
                    this.handleJetConeBoostEntry((JetConeBoostEntry) entry);
                } else if (entry.getEvent() == Event.RefuelAll) {
                    this.handleRefuelAllEntry((RefuelAllEntry) entry);
                } else if (entry.getEvent() == Event.SupercruiseEntry) {
                    this.handleSupercruiseEntryEntry((SupercruiseEntryEntry) entry);
                } else if (entry.getEvent() == Event.SupercruiseExit) {
                    this.handleSupercruiseExitEntry((SupercruiseExitEntry) entry);
                } else if (entry.getEvent() == Event.Touchdown) {
                    this.handleTouchdownEntry((TouchdownEntry) entry);
                } else if (entry.getEvent() == Event.Liftoff) {
                    this.handleLiftoffEntry((LiftoffEntry) entry);
                } else if (entry.getEvent() == Event.Docked) {
                    this.handleDockedEntry((DockedEntry) entry);
                } else if (entry.getEvent() == Event.Undocked) {
                    this.handleUndockedEntry((UndockedEntry) entry);
                } else if (entry.getEvent() == Event.Scan) {
                    this.handleScanEntry((ScanEntry) entry);
                } else if (entry.getEvent() == Event.SellExplorationData) {
                    this.handleSellExplorationDataEntry((SellExplorationDataEntry) entry);
                } else if (entry.getEvent() == Event.ModuleBuy) {
                    this.handleModuleBuyEntry((ModuleBuyEntry) entry);
                } else if (entry.getEvent() == Event.ModuleSell) {
                    this.handleModuleSellEntry((ModuleSellEntry) entry);
                } else if (entry.getEvent() == Event.ModuleSellRemote) {
                    this.handleModuleSellRemoteEntry((ModuleSellRemoteEntry) entry);
                } else if (entry.getEvent() == Event.FetchRemoteModule) {
                    this.handleFetchRemoteModuleEntry((FetchRemoteModuleEntry) entry);
                } else if (entry.getEvent() == Event.ModuleRetrieve) {
                    this.handleModuleRetrieveEntry((ModuleRetrieveEntry) entry);
                } else if (entry.getEvent() == Event.ModuleStore) {
                    this.handleModuleStoreEntry((ModuleStoreEntry) entry);
                } else if (entry.getEvent() == Event.ModuleSwap) {
                    this.handleModuleSwapEntry((ModuleSwapEntry) entry);
                } else if (entry.getEvent() == Event.ShipyardBuy) {
                    this.handleShipyardBuyEntry((ShipyardBuyEntry) entry); // Has only shipType and buyPrice :-( ShipyardNew has the shipID...
                } else if (entry.getEvent() == Event.ShipyardNew) {
                    this.handleShipyardNewEntry((ShipyardNewEntry) entry);
                } else if (entry.getEvent() == Event.ShipyardSwap) {
                    this.handleShipyardSwapEntry((ShipyardSwapEntry) entry);
                } else if (entry.getEvent() == Event.LoadGame) {
                    this.handleLoadGameEntry((LoadGameEntry) entry);
                } else {
                    logger.warn("Is " + entry.getEvent() + " really meaningless?");
                }
            } catch (Exception e) {
                logger.error("Failed to handle " + entry, e);
            }
        }
    }

    private void handleModuleBuyEntry(ModuleBuyEntry e) {
        ShipModule newModule = new ShipModule(e.getBuyItem(), e.getBuyItemLocalized(), e.getBuyPrice());
        ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
        this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleModuleSellEntry(ModuleSellEntry e) {
        ShipModule newModule = null;
        ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
        this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleModuleSellRemoteEntry(ModuleSellRemoteEntry e) {
        this.getStoredModules().sellRemote(e.getSellItem(), e.getSellPrice());
        this.saveStoredModules(this.getCommander(), this.getStoredModules());
    }

    private void handleFetchRemoteModuleEntry(FetchRemoteModuleEntry e) {
        this.getStoredModules().fetchRemote(this.getLastStation(), e.getStoredItem(), e.getTransferCost());
        this.saveStoredModules(this.getCommander(), this.getStoredModules());
    }

    private void handleModuleRetrieveEntry(ModuleRetrieveEntry e) {
        ShipModule newModule = this.getStoredModules().retrieve(this.getLastStation(), e.getRetrievedItem(), e.getRetrievedItemLocalized());
        ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
        this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
        this.getStoredModules().store(this.getLastStation(), oldModule);
        this.saveStoredModules(this.getCommander(), this.getStoredModules());
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleModuleStoreEntry(ModuleStoreEntry e) {
        ShipModule newModule = null;
        if ("Armour".equals(e.getSlot()) || "FrameShiftDrive".equals(e.getSlot()) || "FuelTank".equals(e.getSlot()) || "LifeSupport".equals(e.getSlot()) || "MainEngines".equals(e.getSlot()) || "PowerDistributor".equals(e.getSlot())
                || "PowerPlant".equals(e.getSlot()) || "Radar".equals(e.getSlot())) {
            // Class E default for core internals
            newModule = new ShipModule(e.getStoredItem().replaceAll("_class\\d_", "_class1_"), e.getStoredItemLocalized(), null);
        }
        ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
        this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
        this.getStoredModules().store(this.getLastStation(), oldModule);
        this.saveStoredModules(this.getCommander(), this.getStoredModules());
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleModuleSwapEntry(ModuleSwapEntry e) {
        ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().remove(e.getToSlot());
        ShipModule newModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getFromSlot(), oldModule);
        this.getCurrentShipLoadout().getModulesBySlot().put(e.getToSlot(), newModule);
        this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipModuleChanged(e.getToSlot(), oldModule, newModule);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipModuleChanged(e.getFromSlot(), newModule, oldModule);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleShipyardBuyEntry(ShipyardBuyEntry e) {
        ShipLoadout oldLoadout = this.getCurrentShipLoadout();
        this.saveShipLoadout(this.getCommander(), oldLoadout);
        this.setCurrentShipID(null);
        this.setCurrentShipType(e.getShipType());
        this.setCurrentShipLoadout(new ShipLoadout(null, e.getShipType(), e.getShipPrice()));
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipChanged(oldLoadout, this.getCurrentShipLoadout());
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleShipyardNewEntry(ShipyardNewEntry e) {
        ShipLoadout oldLoadout = this.getCurrentShipLoadout();
        this.setCurrentShipID(e.getNewShipID());
        this.setCurrentShipType(e.getShipType());
        if (this.getCurrentShipLoadout() != null && this.getCurrentShipLoadout().getShipID() == null && e.getShipType().equals(this.getCurrentShipLoadout().getShipType())) {
            ShipLoadout savedLoadout = this.loadShipLoadout(this.getCommander(), e.getNewShipID(), e.getShipType());
            if (savedLoadout != null) {
                this.setCurrentShipLoadout(savedLoadout);
            } else {
                this.getCurrentShipLoadout().setShipID(e.getNewShipID());
            }
        } else {
            this.setCurrentShipLoadout(new ShipLoadout(e.getNewShipID(), e.getShipType(), 0));
        }
        this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipChanged(oldLoadout, this.getCurrentShipLoadout());
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleShipyardSwapEntry(ShipyardSwapEntry e) {
        ShipLoadout oldLoadout = this.getCurrentShipLoadout();
        try {
            this.saveShipLoadout(this.getCommander(), oldLoadout);
        } catch (Exception ex) {
            logger.warn("Failed to save ship loadout " + this.getCommander() + " #" + oldLoadout.getShipID() + " (" + oldLoadout.getShipType() + ")", ex);
        }
        this.setCurrentShipID(e.getShipID());
        this.setCurrentShipType(e.getShipType());
        this.setCurrentShipLoadout(this.loadShipLoadout(this.getCommander(), e.getShipID(), e.getShipType()));
        for (JournalListener listener : this.listeners) {
            try {
                listener.onShipChanged(oldLoadout, this.getCurrentShipLoadout());
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleLoadGameEntry(LoadGameEntry e) {
        this.setCommander(e.getCommander());
        this.setGameMode(e.getGameMode());
        this.setGroup(e.getGroup());
        if ("TestBuggy".equalsIgnoreCase(e.getShip())) {
            // Do not change the current ship. The game has been quit in the SRV.
            // If we now dock the SRV back to the ship we will get a journal entry for that, but that
            // entry won't tell us what the ship is. It should however be the ship that has been used
            // before deploying the SRV, so simply do not change to TestBuggy on game load ;-)
        } else {
            this.setCurrentShipID(e.getShipID());
            this.setCurrentShipType(e.getShip());
            this.setCurrentShipLoadout(this.loadShipLoadout(e.getCommander(), e.getShipID(), e.getShip()));
        }
        this.setStoredModules(this.loadStoredModules(e.getCommander()));
        this.setLastStation(this.loadLastStation(e.getCommander()));
        for (JournalListener listener : this.listeners) {
            try {
                listener.onGameLoaded(this.getCommander(), this.getGameMode(), this.getGroup(), this.getCurrentShipLoadout());
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleSellExplorationDataEntry(SellExplorationDataEntry e) {
        for (String systemName : e.getSystems()) {
            for (int i = this.visitedSystems.size() - 1; i >= 0; i--) {
                VisitedSystem visitedSystem = this.visitedSystems.get(i);
                if (visitedSystem.getSystemName().equals(systemName)) {
                    boolean payedOut = false;
                    if (visitedSystem.getRemainingPayout() != 0) {
                        visitedSystem.setToPayedOut();
                        payedOut = true;
                    }
                    for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                        this.unsetAssumedFirstDiscovery(scannedBody.getBodyName());
                        if (e.getDiscovered().contains(scannedBody.getBodyName())) {
                            scannedBody.setToFirstDiscovered();
                        }
                        scannedBody.setToPayedOut();
                        payedOut = true;
                    }
                    if (payedOut) {
                        break;
                    }
                }
            }
        }
        if (this.estimateRemainingExplorationPayout() < 500000) {
            // Most likely we have sold everything
            for (VisitedSystem visitedSystem : this.visitedSystems) {
                visitedSystem.setToPayedOut();
                for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                    scannedBody.setToPayedOut();
                }
            }
        }
        for (JournalListener listener : this.listeners) {
            try {
                listener.onExplorationDataSold(e);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleScanEntry(ScanEntry e) {
        ScannedBody scannedBody = new ScannedBody(e);
        this.setBodyName(scannedBody.getBodyName());
        this.setBodyType(scannedBody.getBodyType());
        this.visitedSystems.getLast().getScannedBodies().addLast(scannedBody);
        this.scannedBodyNames.add(scannedBody.getBodyName());
        if (this.assumedFirstDiscoveries.contains(scannedBody.getBodyName())) {
            scannedBody.setToFirstDiscovered();
        }
        for (JournalListener listener : this.listeners) {
            try {
                listener.onBodyScanned(scannedBody);
                listener.onLocationChanged(false);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleUndockedEntry(UndockedEntry e) {
        this.setBodyName(e.getStationName());
        this.setBodyType(e.getStationType());
        this.setInSupercruise(false);
        this.setLanded(false);
        for (JournalListener listener : this.listeners) {
            try {
                listener.onLocationChanged(false);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleDockedEntry(DockedEntry e) {
        this.setSystemName(e.getStarSystem());
        this.setBodyName(e.getStationName());
        this.setBodyType(e.getStationType());
        if (StringUtils.isNotEmpty(e.getStationFaction())) {
            this.setFaction(e.getStationFaction());
        }
        if (StringUtils.isNotEmpty(e.getStationAllegiance())) {
            this.setAllegiance(e.getStationAllegiance());
        }
        if (StringUtils.isNotEmpty(e.getStationEconomyLocalized())) {
            this.setEconomy(e.getStationEconomyLocalized());
        }
        if (StringUtils.isNotEmpty(e.getStationGovernmentLocalized())) {
            this.setGovernment(e.getStationGovernmentLocalized());
        }
        this.setInSupercruise(false);
        this.setLanded(true);
        this.setLastStation(e.getStationName());
        for (JournalListener listener : this.listeners) {
            try {
                listener.onLocationChanged(false);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleLiftoffEntry(LiftoffEntry e) {
        this.setLatitude(e.getLatitude());
        this.setLongitude(e.getLongitude());
        this.setInSupercruise(false);
        this.setLanded(false);
        for (JournalListener listener : this.listeners) {
            try {
                listener.onLocationChanged(false);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleTouchdownEntry(TouchdownEntry e) {
        this.setLatitude(e.getLatitude());
        this.setLongitude(e.getLongitude());
        this.setInSupercruise(false);
        this.setLanded(true);
        for (JournalListener listener : this.listeners) {
            try {
                listener.onLocationChanged(false);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleSupercruiseExitEntry(SupercruiseExitEntry e) {
        this.setSystemName(e.getStarSystem());
        this.setBodyName(e.getBody());
        this.setBodyType(e.getBodyType());
        this.setInSupercruise(false);
        this.setLanded(false);
        for (JournalListener listener : this.listeners) {
            try {
                listener.onLocationChanged(false);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleSupercruiseEntryEntry(SupercruiseEntryEntry e) {
        this.setSystemName(e.getStarSystem());
        this.setBodyName(null);
        this.setBodyType(null);
        this.setLatitude(null);
        this.setLongitude(null);
        this.setInSupercruise(true);
        this.setLanded(false);
        for (JournalListener listener : this.listeners) {
            try {
                listener.onLocationChanged(false);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleRefuelAllEntry(RefuelAllEntry e) {
        this.setFuelLevel(this.getFuelLevel() + MiscUtil.getAsFloat(e.getAmount(), 0f));
        if (Math.abs(this.getFuelCapacity() - this.getFuelLevel()) <= 2f) {
            this.setFuelLevel(this.getFuelCapacity()); // Assume fully refueled if close to max. There is always some inprecision due to fuel ticking down by time or launching fuel transfer drones.
        }
        if (this.currentShip != null) {
            this.currentShip.setFuelLevel(this.getFuelLevel());
        }
        for (JournalListener listener : this.listeners) {
            try {
                listener.onFuelLevelChanged(this.getFuelLevel());
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleJetConeBoostEntry(JetConeBoostEntry e) {
        this.setBoostLevel(e.getBoostValue());
        for (JournalListener listener : this.listeners) {
            try {
                listener.onFuelLevelChanged(this.getFuelLevel());
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleFuelScoopEntry(FuelScoopEntry e) {
        this.setFuelLevel(MiscUtil.getAsFloat(e.getTotal(), 0f));
        if (this.currentShip != null) {
            this.currentShip.setFuelLevel(this.getFuelLevel());
        }
        for (JournalListener listener : this.listeners) {
            try {
                listener.onFuelLevelChanged(this.getFuelLevel());
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleFSDJumpEntry(FSDJumpEntry e) {
        this.setCoord(e.getStarPos());
        this.setSystemName(e.getStarSystem());
        this.setBodyName(null);
        this.setBodyType(null);
        this.setFaction(e.getSystemFaction());
        this.setAllegiance(e.getSystemAllegiance());
        this.setEconomy(e.getSystemEconomyLocalized());
        this.setState(e.getFactionState());
        this.setGovernment(e.getSystemGovernmentLocalized());
        this.setSecurity(e.getSystemSecurityLocalized());
        this.setInSupercruise(true);
        this.setLanded(false);
        this.setFuelLevel(MiscUtil.getAsFloat(e.getFuelLevel(), 0f));
        this.setBoostLevel(1f);
        VisitedSystem visitedSystem = new VisitedSystem(e);
        if (visitedSystem.isUninhabited() && !this.visitedSystemNames.contains(visitedSystem.getSystemName())) {
            if (visitedSystem.getCoord().distanceTo(new Coord(0, 0, 0)) > 200) {
                visitedSystem.setAverageJumpPayout();
            }
        }
        this.visitedSystems.addLast(visitedSystem);
        this.visitedSystemNames.add(visitedSystem.getSystemName());
        if (this.currentShip != null) {
            this.currentShip.setFuelLevel(this.getFuelLevel());
        }
        for (JournalListener listener : this.listeners) {
            try {
                listener.onLocationChanged(true);
                listener.onFuelLevelChanged(this.getFuelLevel());
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    private void handleLocationEntry(LocationEntry e) {
        this.setCoord(e.getStarPos());
        this.setSystemName(e.getStarSystem());
        this.setBodyName(e.getBody());
        this.setBodyType(e.getBodyType());
        this.setStationName(e.getStationName());
        this.setStationType(e.getStationType());
        this.setFaction(e.getSystemFaction());
        this.setAllegiance(e.getSystemAllegiance());
        this.setEconomy(e.getSystemEconomyLocalized());
        this.setState(e.getFactionState());
        this.setGovernment(e.getSystemGovernmentLocalized());
        this.setSecurity(e.getSystemSecurityLocalized());
        this.setLanded(Boolean.TRUE.equals(e.getDocked()));
        for (JournalListener listener : this.listeners) {
            try {
                listener.onLocationChanged(false);
            } catch (Exception ex) {
                logger.warn(listener + " failed: " + ex);
            }
        }
    }

    public LinkedList<AbstractJournalEntry> getEntries() {
        return this.getEntries(null, null);
    }

    /**
     * @param fromDate inclusive (can be null)
     * @param toDate exclusive (can be null)
     * @param events type of events or null/empty for any type
     * @return Journal entries, sorted ascending by date
     */
    public LinkedList<AbstractJournalEntry> getEntries(Date fromDate, Date toDate, Event... events) {
        final Set<Event> eventsAsSet = events == null || events.length == 0 ? Collections.emptySet() : new HashSet<>(Arrays.asList(events));

        if (fromDate == null && toDate == null && eventsAsSet.isEmpty()) {
            return this.entries;
        } else {
            LinkedList<AbstractJournalEntry> result = new LinkedList<>();

            for (AbstractJournalEntry e : this.entries) {
                if (fromDate == null || e.getTimestamp().compareTo(fromDate) >= 0) {
                    if (toDate == null || e.getTimestamp().compareTo(toDate) < 0) {
                        if (eventsAsSet.isEmpty() || eventsAsSet.contains(e.getEvent())) {
                            result.add(e);
                        }
                    }
                }
            }

            return result;
        }
    }

    public Coord getCoord() {
        return this.coord;
    }

    public void setCoord(Coord coord) {
        this.coord = coord;
    }

    public String getSystemName() {
        return this.systemName;
    }

    public void setSystemName(String systemName) {
        this.systemName = systemName;
    }

    public String getBodyName() {
        return this.bodyName;
    }

    public void setBodyName(String bodyName) {
        this.bodyName = bodyName;
    }

    public String getBodyType() {
        return this.bodyType;
    }

    public void setBodyType(String bodyType) {
        this.bodyType = bodyType;
    }

    public String getStationName() {
        return this.stationName;
    }

    public void setStationName(String stationName) {
        this.stationName = stationName;
    }

    public String getStationType() {
        return this.stationType;
    }

    public void setStationType(String stationType) {
        this.stationType = stationType;
    }

    public Float getLatitude() {
        return this.latitude;
    }

    public void setLatitude(Float latitude) {
        this.latitude = latitude;
    }

    public Float getLongitude() {
        return this.longitude;
    }

    public void setLongitude(Float longitude) {
        this.longitude = longitude;
    }

    public String getFaction() {
        return this.faction;
    }

    public void setFaction(String faction) {
        this.faction = faction;
    }

    public String getAllegiance() {
        return this.allegiance;
    }

    public void setAllegiance(String allegiance) {
        this.allegiance = allegiance;
    }

    public String getEconomy() {
        return this.economy;
    }

    public void setEconomy(String economy) {
        this.economy = economy;
    }

    public String getState() {
        return this.state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getGovernment() {
        return this.government;
    }

    public void setGovernment(String government) {
        this.government = government;
    }

    public String getSecurity() {
        return this.security;
    }

    public void setSecurity(String security) {
        this.security = security;
    }

    public boolean isInSupercruise() {
        return this.inSupercruise;
    }

    public void setInSupercruise(boolean inSupercruise) {
        this.inSupercruise = inSupercruise;
    }

    public boolean isLanded() {
        return this.landed;
    }

    public void setLanded(boolean landed) {
        this.landed = landed;
    }

    public float getFuelLevel() {
        return this.fuelLevel;
    }

    public void setFuelLevel(float fuelLevel) {
        this.fuelLevel = fuelLevel;
    }

    public float getBoostLevel() {
        return this.boostLevel;
    }

    public void setBoostLevel(float boostLevel) {
        this.boostLevel = boostLevel;
    }

    public String getCommander() {
        return this.commander;
    }

    public void setCommander(String commander) {
        this.commander = commander;
    }

    public String getGameMode() {
        return this.gameMode;
    }

    public void setGameMode(String gameMode) {
        this.gameMode = gameMode;
    }

    public String getGroup() {
        return this.group;
    }

    public void setGroup(String group) {
        this.group = group;
    }

    public Integer getCurrentShipID() {
        return this.currentShipID;
    }

    public void setCurrentShipID(Integer currentShipID) {
        this.currentShipID = currentShipID;
    }

    public String getCurrentShipType() {
        return this.currentShipType;
    }

    public void setCurrentShipType(String currentShipType) {
        this.currentShipType = currentShipType;
    }

    public ShipLoadout getCurrentShipLoadout() {
        return this.currentShipLoadout;
    }

    public void setCurrentShipLoadout(ShipLoadout currentShipLoadout) {
        this.currentShipLoadout = currentShipLoadout;
    }

    public StoredModules getStoredModules() {
        return this.storedModules;
    }

    public void setStoredModules(StoredModules storedModules) {
        this.storedModules = storedModules;
    }

    public String getLastStation() {
        return this.lastStation;
    }

    public void setLastStation(String lastStation) {
        this.lastStation = lastStation;
    }

    public SortedMap<String, SortedMap<String, Integer>> getOffsetByName() {
        return this.offsetByName;
    }

    public void setOffsetByName(SortedMap<String, SortedMap<String, Integer>> offsetByName) {
        this.offsetByName = offsetByName;
    }

    public SortedMap<String, SortedMap<String, Integer>> getHaveByName() {
        return this.haveByName;
    }

    public void setHaveByName(SortedMap<String, SortedMap<String, Integer>> haveByName) {
        this.haveByName = haveByName;
    }

    public SortedMap<String, SortedMap<String, Integer>> getCollectedByName() {
        return this.collectedByName;
    }

    public void setCollectedByName(SortedMap<String, SortedMap<String, Integer>> collectedByName) {
        this.collectedByName = collectedByName;
    }

    public SortedMap<String, SortedMap<String, Integer>> getDiscardedByName() {
        return this.discardedByName;
    }

    public void setDiscardedByName(SortedMap<String, SortedMap<String, Integer>> discardedByName) {
        this.discardedByName = discardedByName;
    }

    public SortedMap<String, SortedMap<String, Integer>> getSpentByName() {
        return this.spentByName;
    }

    public void setSpentByName(SortedMap<String, SortedMap<String, Integer>> spentByName) {
        this.spentByName = spentByName;
    }

    public SortedMap<String, SortedMap<String, Float>> getPriorityByName() {
        return this.priorityByName;
    }

    public void setPriorityByName(SortedMap<String, SortedMap<String, Float>> priorityByName) {
        this.priorityByName = priorityByName;
    }

    public SortedMap<String, SortedMap<String, Integer>> getSurplusByName() {
        return this.surplusByName;
    }

    public void setSurplusByName(SortedMap<String, SortedMap<String, Integer>> surplusByName) {
        this.surplusByName = surplusByName;
    }

    public SortedMap<String, SortedMap<String, Integer>> getRequiredByName() {
        return this.requiredByName;
    }

    public void setRequiredByName(SortedMap<String, SortedMap<String, Integer>> requiredByName) {
        this.requiredByName = requiredByName;
    }

    public boolean isSystemVisited(String systemName) {
        return false; // TODO
    }

    public boolean isBodyScanned(String bodyName) {
        return false; // TODO
    }

}
