package borg.edtrading.sidepanel;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.ModuleData;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.fleet.ModuleBuyEntry;
import borg.edtrading.journal.entries.fleet.ModuleRetrieveEntry;
import borg.edtrading.journal.entries.fleet.ModuleSellEntry;
import borg.edtrading.journal.entries.fleet.ModuleStoreEntry;
import borg.edtrading.journal.entries.fleet.ModuleSwapEntry;
import borg.edtrading.journal.entries.fleet.ShipyardBuyEntry;
import borg.edtrading.journal.entries.fleet.ShipyardNewEntry;
import borg.edtrading.journal.entries.fleet.ShipyardSwapEntry;
import borg.edtrading.journal.entries.game.LoadGameEntry;
import borg.edtrading.journal.entries.game.LoadoutEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * GameSession
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GameSession implements JournalUpdateListener, Serializable {

    private static final long serialVersionUID = -4069666534861037328L;

    static final Logger logger = LogManager.getLogger(GameSession.class);

    private String commander = null;
    private String gameMode = null;
    private String group = null;
    private Integer currentShipID = null;
    private String currentShipType = null;
    private ShipLoadout currentShipLoadout = null;

    private final List<GameSessionListener> listeners = new ArrayList<>();

    public GameSession(JournalReaderThread journalReaderThread) {
        if (journalReaderThread != null) {
            journalReaderThread.addListener(this);
        }
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

    @Override
    public void onNewJournalLine(String line) {
        // Do nothing
    }

    @Override
    public void onNewJournalEntry(AbstractJournalEntry entry) {
        try {
            if (entry.getEvent() == Event.Loadout) {
                LoadoutEntry e = (LoadoutEntry) entry;
                this.setCurrentShipID(e.getShipID());
                this.setCurrentShipType(e.getShip());
                ShipLoadout shipLoadout = new ShipLoadout(e.getShipID(), e.getShip(), e.getShipIdent(), e.getShipName());
                if (e.getModules() != null) {
                    for (ModuleData data : e.getModules()) {
                        ShipModule module = new ShipModule(data.getItem().toLowerCase(), data.getItem(), data.getValue());
                        shipLoadout.getModulesBySlot().put(data.getSlot(), module);
                    }
                }
                this.setCurrentShipLoadout(shipLoadout);
            } else if (entry.getEvent() == Event.ModuleBuy) {
                ModuleBuyEntry e = (ModuleBuyEntry) entry;
                ShipModule newModule = new ShipModule(e.getBuyItem().toLowerCase(), e.getBuyItemLocalized(), e.getBuyPrice());
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
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
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(e.getSlot(), oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ModuleSellRemote) {
                //                ModuleSellRemoteEntry e = (ModuleSellRemoteEntry) entry;
                //                this.getStoredModules().sellRemote(e.getSellItem(), e.getSellPrice());
                //                this.saveStoredModules(this.getCommander(), this.getStoredModules());
            } else if (entry.getEvent() == Event.FetchRemoteModule) {
                //                FetchRemoteModuleEntry e = (FetchRemoteModuleEntry) entry;
                //                this.getStoredModules().fetchRemote(this.getLastStation(), e.getStoredItem(), e.getTransferCost());
                //                this.saveStoredModules(this.getCommander(), this.getStoredModules());
            } else if (entry.getEvent() == Event.ModuleRetrieve) {
                ModuleRetrieveEntry e = (ModuleRetrieveEntry) entry;
                ShipModule newModule = new ShipModule(e.getRetrievedItem().toLowerCase(), e.getRetrievedItemLocalized(), null);
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
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
                    newModule = new ShipModule(e.getStoredItem().toLowerCase().replaceAll("_class\\d_", "_class1_"), e.getStoredItemLocalized(), null);
                }
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
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
                    this.getCurrentShipLoadout().setShipID(e.getNewShipID());
                } else {
                    this.setCurrentShipLoadout(new ShipLoadout(e.getNewShipID(), e.getShipType(), 0));
                }
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
                this.setCurrentShipID(e.getShipID());
                this.setCurrentShipType(e.getShipType());
                this.setCurrentShipLoadout(new ShipLoadout(e.getShipID(), e.getShipType(), 0));
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
                    if (this.getCurrentShipLoadout() == null) {
                        this.setCurrentShipLoadout(new ShipLoadout(e.getShipID(), e.getShip(), e.getShipIdent(), e.getShipName()));
                    }
                    this.getCurrentShipLoadout().setFuelLevel(e.getFuelLevel());
                }
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

}
