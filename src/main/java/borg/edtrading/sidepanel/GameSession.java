package borg.edtrading.sidepanel;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.entries.AbstractJournalEntry;
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
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * GameSession
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GameSession implements Serializable {

    private static final long serialVersionUID = -4069666534861037328L;

    static final Logger logger = LogManager.getLogger(GameSession.class);

    private String commander = null;
    private String gameMode = null;
    private String group = null;
    private Integer currentShipID = null;
    private String currentShipType = null;
    private ShipLoadout currentShipLoadout = null;
    private StoredModules storedModules = null;
    private String lastStation = null;

    private final List<GameSessionListener> listeners = new ArrayList<>();

    public GameSession(JournalReaderThread journalReaderThread) {
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

    public boolean addListener(GameSessionListener listener) {
        if (listener == null || this.listeners.contains(listener)) {
            return false;
        } else {
            return this.listeners.add(listener);
        }
    }

    public boolean removeListener(GameSessionListener listener) {
        if (listener == null) {
            return false;
        } else {
            return this.listeners.remove(listener);
        }
    }

    private void onNewJournalEntry(AbstractJournalEntry entry) {
        try {
            if (entry.getEvent() == Event.Docked) {
                DockedEntry e = (DockedEntry) entry;
                this.setLastStation(e.getStationName());
            } else if (entry.getEvent() == Event.ModuleBuy) {
                ModuleBuyEntry e = (ModuleBuyEntry) entry;
                ShipModule newModule = new ShipModule(e.getBuyItem(), e.getBuyItemLocalized(), e.getBuyPrice());
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
                this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ModuleSell) {
                ModuleSellEntry e = (ModuleSellEntry) entry;
                ShipModule newModule = null;
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
                this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ModuleSellRemote) {
                ModuleSellRemoteEntry e = (ModuleSellRemoteEntry) entry;
                this.getStoredModules().sellRemote(e.getSellItem(), e.getSellPrice());
                this.saveStoredModules(this.getCommander(), this.getStoredModules());
            } else if (entry.getEvent() == Event.FetchRemoteModule) {
                FetchRemoteModuleEntry e = (FetchRemoteModuleEntry) entry;
                this.getStoredModules().fetchRemote(this.getLastStation(), e.getStoredItem(), e.getTransferCost());
                this.saveStoredModules(this.getCommander(), this.getStoredModules());
            } else if (entry.getEvent() == Event.ModuleRetrieve) {
                ModuleRetrieveEntry e = (ModuleRetrieveEntry) entry;
                ShipModule newModule = this.getStoredModules().retrieve(this.getLastStation(), e.getRetrievedItem(), e.getRetrievedItemLocalized());
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
                this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
                this.getStoredModules().store(this.getLastStation(), oldModule);
                this.saveStoredModules(this.getCommander(), this.getStoredModules());
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ModuleStore) {
                ModuleStoreEntry e = (ModuleStoreEntry) entry;
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
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ModuleSwap) {
                ModuleSwapEntry e = (ModuleSwapEntry) entry;
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().remove(e.getToSlot());
                ShipModule newModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getFromSlot(), oldModule);
                this.getCurrentShipLoadout().getModulesBySlot().put(e.getToSlot(), newModule);
                this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(e.getToSlot(), oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(e.getFromSlot(), newModule, oldModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ShipyardBuy) {
                ShipyardBuyEntry e = (ShipyardBuyEntry) entry; // Has only shipType and buyPrice :-( ShipyardNew has the shipID...
                ShipLoadout oldLoadout = this.getCurrentShipLoadout();
                this.saveShipLoadout(this.getCommander(), oldLoadout);
                this.setCurrentShipID(null);
                this.setCurrentShipType(e.getShipType());
                this.setCurrentShipLoadout(new ShipLoadout(null, e.getShipType(), e.getShipPrice()));
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipChanged(oldLoadout, this.getCurrentShipLoadout());
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ShipyardNew) {
                ShipyardNewEntry e = (ShipyardNewEntry) entry;
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
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipChanged(oldLoadout, this.getCurrentShipLoadout());
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ShipyardSwap) {
                ShipyardSwapEntry e = (ShipyardSwapEntry) entry;
                ShipLoadout oldLoadout = this.getCurrentShipLoadout();
                try {
                    this.saveShipLoadout(this.getCommander(), oldLoadout);
                } catch (Exception ex) {
                    logger.warn("Failed to save ship loadout " + this.getCommander() + " #" + oldLoadout.getShipID() + " (" + oldLoadout.getShipType() + ")", ex);
                }
                this.setCurrentShipID(e.getShipID());
                this.setCurrentShipType(e.getShipType());
                this.setCurrentShipLoadout(this.loadShipLoadout(this.getCommander(), e.getShipID(), e.getShipType()));
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipChanged(oldLoadout, this.getCurrentShipLoadout());
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.LoadGame) {
                LoadGameEntry e = (LoadGameEntry) entry;
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
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onGameLoaded(this.getCommander(), this.getGameMode(), this.getGroup(), this.getCurrentShipLoadout());
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            }
        } catch (Exception e) {
            logger.error("Failed to handle " + entry, e);
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

}
