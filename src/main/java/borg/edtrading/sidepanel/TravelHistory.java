package borg.edtrading.sidepanel;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.exploration.ScanEntry;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
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
import borg.edtrading.util.MiscUtil;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * TravelHistory
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TravelHistory implements JournalUpdateListener, GameSessionListener, Serializable {

    private static final long serialVersionUID = 3845858336099915390L;

    static final Logger logger = LogManager.getLogger(TravelHistory.class);

    private transient ShipLoadout currentShip = null;

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
    private float boostLevel = 1;

    private final Set<String> assumedFirstDiscoveries = new TreeSet<>();
    private final LinkedList<VisitedSystem> visitedSystems = new LinkedList<>();
    private final Set<String> visitedSystemNames = new HashSet<>();

    private final List<TravelHistoryListener> listeners = new ArrayList<>();

    public TravelHistory(JournalReaderThread journalReaderThread, GameSession gameSession) {
        this.loadAssumedFirstDiscoveries();
        if (journalReaderThread != null) {
            journalReaderThread.addListener(this);
        }
        if (gameSession != null) {
            gameSession.addListener(this);
        }
    }

    public int estimateRemainingExplorationPayout() {
        int estimate = 0;
        for (VisitedSystem visitedSystem : this.getVisitedSystems()) {
            estimate += visitedSystem.getRemainingPayout();
            for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                estimate += scannedBody.getRemainingBasePayout();
                estimate += scannedBody.getRemainingBonusPayout();
            }
        }
        return estimate;
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

    public float getBoostLevel() {
        return this.boostLevel;
    }

    public void setBoostLevel(float boostLevel) {
        this.boostLevel = boostLevel;
    }

    public LinkedList<VisitedSystem> getVisitedSystems() {
        return this.visitedSystems;
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

    public void setToAssumedFirstDiscovery(String bodyName) {
        if (bodyName != null) {
            if (this.assumedFirstDiscoveries.add(bodyName)) {
                this.saveAssumedFirstDiscoveries();

                for (int i = this.visitedSystems.size() - 1; i >= 0; i--) {
                    VisitedSystem visitedSystem = this.visitedSystems.get(i);
                    for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                        if (scannedBody.getBodyName().equals(bodyName)) {
                            scannedBody.setToFirstDiscovered();
                            return;
                        }
                    }
                }
            }
        }
    }

    public void unsetAssumedFirstDiscovery(String bodyName) {
        if (bodyName != null) {
            if (this.assumedFirstDiscoveries.remove(bodyName)) {
                this.saveAssumedFirstDiscoveries();

                for (int i = this.visitedSystems.size() - 1; i >= 0; i--) {
                    VisitedSystem visitedSystem = this.visitedSystems.get(i);
                    for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                        if (scannedBody.getBodyName().equals(bodyName)) {
                            scannedBody.setToNotFirstDiscovered();
                            return;
                        }
                    }
                }
            }
        }
    }

    public void saveAssumedFirstDiscoveries() {
        try {
            File dir = new File(System.getProperty("user.home"), ".edsidepanel");
            if (!dir.exists()) {
                dir.mkdirs();
            }
            File file = new File(dir, "AssumedFirstDiscoveries.txt");
            FileUtils.write(file, this.assumedFirstDiscoveries.stream().collect(Collectors.joining("\n")), "UTF-8", false);
        } catch (IOException e) {
            logger.error("Failed to save assumed first discoveries", e);
        }
    }

    public void loadAssumedFirstDiscoveries() {
        try {
            File dir = new File(System.getProperty("user.home"), ".edsidepanel");
            File file = new File(dir, "AssumedFirstDiscoveries.txt");
            if (file.exists() && file.length() > 0) {
                this.assumedFirstDiscoveries.clear();
                for (String line : FileUtils.readLines(file, "UTF-8")) {
                    this.assumedFirstDiscoveries.add(line);
                }
            }
        } catch (IOException e) {
            logger.error("Failed to load assumed first discoveries", e);
        }
    }

    @Override
    public void onNewJournalLine(String line) {
        // Do nothing
    }

    @Override
    public void onNewJournalEntry(AbstractJournalEntry entry) {
        try {
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
                if (this.currentShip != null) {
                    this.currentShip.setFuelLevel(this.getFuelLevel());
                }
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onFuelLevelChanged(this.getFuelLevel());
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.JetConeBoost) {
                JetConeBoostEntry e = (JetConeBoostEntry) entry;
                this.setBoostLevel(e.getBoostValue());
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
                if (this.currentShip != null) {
                    this.currentShip.setFuelLevel(this.getFuelLevel());
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
            } else if (entry.getEvent() == Event.Scan) {
                ScanEntry e = (ScanEntry) entry;
                ScannedBody scannedBody = new ScannedBody(e);
                this.setBodyName(scannedBody.getBodyName());
                this.setBodyType(scannedBody.getBodyType());
                this.visitedSystems.getLast().getScannedBodies().addLast(scannedBody);
                if (this.assumedFirstDiscoveries.contains(scannedBody.getBodyName())) {
                    scannedBody.setToFirstDiscovered();
                }
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onLocationChanged();
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            } else if (entry.getEvent() == Event.SellExplorationData) {
                SellExplorationDataEntry e = (SellExplorationDataEntry) entry;
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
                for (TravelHistoryListener listener : this.listeners) {
                    try {
                        listener.onExplorationDataSold(e);
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
            this.currentShip = null;
        } else {
            this.setFuelCapacity(ship.getFuelCapacity());
            this.setFuelLevel(MiscUtil.getAsFloat(ship.getFuelLevel(), 0f));
            this.currentShip = ship;
        }
    }

    @Override
    public void onShipModuleChanged(String slot, ShipModule oldModule, ShipModule newModule) {
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
            this.currentShip = null;
        } else {
            this.setFuelCapacity(newShip.getFuelCapacity());
            this.setFuelLevel(MiscUtil.getAsFloat(newShip.getFuelLevel(), 0f));
            this.currentShip = newShip;
        }
    }

}
