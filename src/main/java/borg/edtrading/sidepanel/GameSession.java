package borg.edtrading.sidepanel;

import borg.edtrading.journal.AbstractJournalEntry;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.LoadGameEntry;
import borg.edtrading.journal.ModuleBuyEntry;
import borg.edtrading.journal.ModuleRetrieveEntry;
import borg.edtrading.journal.ModuleStoreEntry;
import borg.edtrading.journal.ShipyardBuyEntry;
import borg.edtrading.journal.ShipyardNewEntry;
import borg.edtrading.journal.ShipyardSwapEntry;
import com.google.gson.Gson;
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
            if (entry.getEvent() == Event.ModuleBuy) {
                ModuleBuyEntry e = (ModuleBuyEntry) entry;
                ShipModule newModule = new ShipModule(e.getBuyItem(), e.getBuyItemLocalized(), e.getBuyPrice());
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
                try {
                    this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
                } catch (Exception ex) {
                    logger.warn("Failed to save ship loadout " + this.getCommander() + " #" + this.getCurrentShipLoadout().getShipID() + " (" + this.getCurrentShipLoadout().getShipType() + ")", ex);
                }
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ModuleRetrieve) {
                ModuleRetrieveEntry e = (ModuleRetrieveEntry) entry;
                ShipModule newModule = new ShipModule(e.getRetrievedItem(), e.getRetrievedItemLocalized(), null); // TODO buyPrice
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().put(e.getSlot(), newModule);
                try {
                    this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
                } catch (Exception ex) {
                    logger.warn("Failed to save ship loadout " + this.getCommander() + " #" + this.getCurrentShipLoadout().getShipID() + " (" + this.getCurrentShipLoadout().getShipType() + ")", ex);
                }
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ModuleStore) {
                ModuleStoreEntry e = (ModuleStoreEntry) entry;
                ShipModule oldModule = this.getCurrentShipLoadout().getModulesBySlot().remove(e.getSlot());
                ShipModule newModule = null; // TODO Class E default for core internals
                try {
                    this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
                } catch (Exception ex) {
                    logger.warn("Failed to save ship loadout " + this.getCommander() + " #" + this.getCurrentShipLoadout().getShipID() + " (" + this.getCurrentShipLoadout().getShipType() + ")", ex);
                }
                for (GameSessionListener listener : this.listeners) {
                    try {
                        listener.onShipModuleChanged(oldModule, newModule);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.ShipyardBuy) {
                ShipyardBuyEntry e = (ShipyardBuyEntry) entry; // Has only shipType and buyPrice :-( ShipyardNew has the shipID...
                ShipLoadout oldLoadout = this.getCurrentShipLoadout();
                try {
                    this.saveShipLoadout(this.getCommander(), oldLoadout);
                } catch (Exception ex) {
                    logger.warn("Failed to save ship loadout " + this.getCommander() + " #" + oldLoadout.getShipID() + " (" + oldLoadout.getShipType() + ")", ex);
                }
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
                try {
                    this.saveShipLoadout(this.getCommander(), this.getCurrentShipLoadout());
                } catch (Exception ex) {
                    logger.warn("Failed to save ship loadout " + this.getCommander() + " #" + this.getCurrentShipLoadout().getShipID() + " (" + this.getCurrentShipLoadout().getShipType() + ")", ex);
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
                try {
                    this.saveShipLoadout(this.getCommander(), oldLoadout);
                } catch (Exception ex) {
                    logger.warn("Failed to save ship loadout " + this.getCommander() + " #" + oldLoadout.getShipID() + " (" + oldLoadout.getShipType() + ")", ex);
                }
                this.setCurrentShipID(e.getShipID());
                this.setCurrentShipType(e.getShipType());
                try {
                    this.setCurrentShipLoadout(this.loadShipLoadout(this.getCommander(), e.getShipID(), e.getShipType()));
                    if (this.getCurrentShipLoadout() == null) {
                        this.setCurrentShipLoadout(new ShipLoadout(e.getShipID(), e.getShipType()));
                    }
                } catch (Exception ex) {
                    this.setCurrentShipLoadout(new ShipLoadout(e.getShipID(), e.getShipType()));
                    logger.warn("Failed to load ship loadout " + this.getCommander() + " #" + e.getShipID() + " (" + e.getShipType() + ")", ex);
                }
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
                this.setCurrentShipID(e.getShipID());
                this.setCurrentShipType(e.getShip());
                try {
                    this.setCurrentShipLoadout(this.loadShipLoadout(e.getCommander(), e.getShipID(), e.getShip()));
                    if (this.getCurrentShipLoadout() == null) {
                        this.setCurrentShipLoadout(new ShipLoadout(e.getShipID(), e.getShip()));
                    }
                } catch (Exception ex) {
                    this.setCurrentShipLoadout(new ShipLoadout(e.getShipID(), e.getShip()));
                    logger.warn("Failed to load ship loadout " + e.getCommander() + " #" + e.getShipID() + " (" + e.getShip() + ")", ex);
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

    private ShipLoadout loadShipLoadout(String commander, Integer shipID, String shipType) throws IOException {
        File file = new File(System.getProperty("user.home"), ".ShipLoadout." + commander + "." + shipID + "_" + shipType + ".json");
        if (!file.exists() || file.length() == 0) {
            return null;
        } else {
            String json = FileUtils.readFileToString(file, "UTF-8");
            return new Gson().fromJson(json, ShipLoadout.class);
        }
    }

    private void saveShipLoadout(String commander, ShipLoadout shipLoadout) throws IOException {
        if (shipLoadout != null) {
            File file = new File(System.getProperty("user.home"), ".ShipLoadout." + commander + "." + shipLoadout.getShipID() + "_" + shipLoadout.getShipType() + ".json");
            String json = new Gson().toJson(shipLoadout);
            FileUtils.write(file, json, "UTF-8", false);
        }
    }

}
