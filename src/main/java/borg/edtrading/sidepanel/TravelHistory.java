package borg.edtrading.sidepanel;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.entries.AbstractJournalEntry;
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
import borg.edtrading.util.MiscUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * TravelHistory
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TravelHistory implements JournalUpdateListener, GameSessionListener, Serializable {

    private static final long serialVersionUID = 3845858336099915390L;

    static final Logger logger = LogManager.getLogger(TravelHistory.class);

    private Coord coord = null;
    private String systemName = null;
    private String bodyName = null;
    private String bodyType = null;
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
    private int fuelCapacity = 0;

    private final List<TravelHistoryListener> listeners = new ArrayList<>();

    public TravelHistory(JournalReaderThread journalReaderThread, GameSession gameSession) {
        if (journalReaderThread != null) {
            journalReaderThread.addListener(this);
        }
        if (gameSession != null) {
            gameSession.addListener(this);
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

    public int getFuelCapacity() {
        return this.fuelCapacity;
    }

    public void setFuelCapacity(int fuelCapacity) {
        this.fuelCapacity = fuelCapacity;
    }

    public boolean addListener(TravelHistoryListener listener) {
        if (listener == null || this.listeners.contains(listener)) {
            return false;
        } else {
            return this.listeners.add(listener);
        }
    }

    public boolean removeListener(TravelHistoryListener listener) {
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
            //TODO Scan
            //TODO SellExplorationData
            //TODO SRV/Fighter
            if (entry.getEvent() == Event.Location) {
                LocationEntry e = (LocationEntry) entry;
                this.setCoord(e.getStarPos());
                this.setSystemName(e.getStarSystem());
                this.setBodyName(e.getBody());
                this.setBodyType(e.getBodyType());
                if (StringUtils.isNotEmpty(e.getStationType())) {
                    this.setBodyType(e.getStationType());
                }
                this.setFaction(e.getSystemFaction());
                this.setAllegiance(e.getSystemAllegiance());
                this.setEconomy(e.getSystemEconomyLocalized());
                this.setState(e.getFactionState());
                this.setGovernment(e.getSystemGovernmentLocalized());
                this.setSecurity(e.getSystemSecurityLocalized());
                this.setLanded(Boolean.TRUE.equals(e.getDocked()));
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.FSDJump) {
                FSDJumpEntry e = (FSDJumpEntry) entry;
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
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                        listener.onFuelLevelChanged(this.getFuelLevel());
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.FuelScoop) {
                FuelScoopEntry e = (FuelScoopEntry) entry;
                this.setFuelLevel(MiscUtil.getAsFloat(e.getTotal(), 0f));
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onFuelLevelChanged(this.getFuelLevel());
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.RefuelAll) {
                RefuelAllEntry e = (RefuelAllEntry) entry;
                this.setFuelLevel(this.getFuelLevel() + MiscUtil.getAsFloat(e.getAmount(), 0f));
                if (Math.abs(this.getFuelCapacity() - this.getFuelLevel()) <= 2f) {
                    this.setFuelLevel(this.getFuelCapacity()); // Assume fully refueled if close to max. There is always some inprecision due to fuel ticking down by time or launching fuel transfer drones.
                }
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onFuelLevelChanged(this.getFuelLevel());
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.SupercruiseEntry) {
                SupercruiseEntryEntry e = (SupercruiseEntryEntry) entry;
                this.setSystemName(e.getStarSystem());
                this.setBodyName(null);
                this.setBodyType(null);
                this.setLatitude(null);
                this.setLongitude(null);
                this.setInSupercruise(true);
                this.setLanded(false);
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.SupercruiseExit) {
                SupercruiseExitEntry e = (SupercruiseExitEntry) entry;
                this.setSystemName(e.getStarSystem());
                this.setBodyName(e.getBody());
                this.setBodyType(e.getBodyType());
                this.setInSupercruise(false);
                this.setLanded(false);
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.Touchdown) {
                TouchdownEntry e = (TouchdownEntry) entry;
                this.setLatitude(e.getLatitude());
                this.setLongitude(e.getLongitude());
                this.setInSupercruise(false);
                this.setLanded(true);
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.Liftoff) {
                LiftoffEntry e = (LiftoffEntry) entry;
                this.setLatitude(e.getLatitude());
                this.setLongitude(e.getLongitude());
                this.setInSupercruise(false);
                this.setLanded(false);
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.Docked) {
                DockedEntry e = (DockedEntry) entry;
                this.setSystemName(e.getStarSystem());
                this.setBodyName(e.getStationName());
                this.setBodyType(e.getStationType());
                this.setFaction(e.getStationFaction());
                this.setAllegiance(e.getStationAllegiance());
                this.setEconomy(e.getStationEconomyLocalized());
                this.setGovernment(e.getStationGovernmentLocalized());
                this.setInSupercruise(false);
                this.setLanded(true);
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.Undocked) {
                UndockedEntry e = (UndockedEntry) entry;
                this.setBodyName(e.getStationName());
                this.setBodyType(e.getStationType());
                this.setInSupercruise(false);
                this.setLanded(false);
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            }
        } catch (Exception e) {
            logger.error("Failed to handle " + entry, e);
        }
    }

    @Override
    public void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship) {
        if (ship == null) {
            this.setFuelCapacity(0);
            this.setFuelLevel(0);
        } else {
            this.setFuelCapacity(ship.getFuelCapacity());
            this.setFuelLevel(ship.getFuelCapacity() / 2.0f); // We don't know
        }
    }

    @Override
    public void onShipModuleChanged(ShipModule oldModule, ShipModule newModule) {
        if (oldModule != null && oldModule.getFuelCapacity() != null) {
            this.setFuelCapacity(this.getFuelCapacity() - oldModule.getFuelCapacity());
        }
        if (newModule != null && newModule.getFuelCapacity() != null) {
            this.setFuelCapacity(this.getFuelCapacity() + newModule.getFuelCapacity());
        }
    }

    @Override
    public void onShipChanged(ShipLoadout oldShip, ShipLoadout newShip) {
        if (newShip == null) {
            this.setFuelCapacity(0);
            this.setFuelLevel(0);
        } else {
            this.setFuelCapacity(newShip.getFuelCapacity());
            this.setFuelLevel(newShip.getFuelCapacity() / 2.0f); // We don't know
        }
    }

}
