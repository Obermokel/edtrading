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
import borg.edtrading.util.MiscUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * TravelHistory
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TravelHistory implements JournalUpdateListener, GameSessionListener, Serializable {

    private static final long serialVersionUID = 3845858336099915390L;

    static final Logger logger = LogManager.getLogger(TravelHistory.class);

    static final Map<String, Integer> PAYOUTS = new HashMap<>();
    static {
      //@formatter:off
      PAYOUTS.put(                                   "JUMP", 3302);

      PAYOUTS.put(                                "Class O", 3962);//3962
      PAYOUTS.put(                                "Class B", (3490+3329+3307+3055)/4);//3490+3329+3307+3055
      PAYOUTS.put(                                "Class A", (2985+2954)/2);//2985+2954
      PAYOUTS.put(                                "Class F", (2935+2933+2953)/3);//2935+2933+2953
      PAYOUTS.put(                                "Class G", (2920+2920+2921+2917)/4);//2920+2920+2921+2917
      PAYOUTS.put(                                "Class K", (2904+2912+2917)/3);//2904+2912+2917
      PAYOUTS.put(                       "Class K_RedGiant", 2900);
      PAYOUTS.put(                                "Class M", (2898+2897+2895+2893)/4);//2898+2897+2895+2893
      PAYOUTS.put(                       "Class M_RedGiant", 2900);
      PAYOUTS.put(                                "Class C", 2902);//2902
      PAYOUTS.put(                                "Class L", 2889);//2889
      PAYOUTS.put(                                "Class Y", 2882);//2882
      PAYOUTS.put(                                "Class T", (2883+2884)/2);//2883+2884
      PAYOUTS.put(                              "Class TTS", (2900+2882)/2);//2900+2882

      PAYOUTS.put(                                "Class N", (44087+44044+43765+43357)/4);//44087+44044+43765+43357
      PAYOUTS.put(                                "Class H", (44721+45177+45367)/3);//44721+45177+45367
      PAYOUTS.put(                               "Class DA", (27370+27020)/2);//27370+27020
      PAYOUTS.put(                               "Class DB", 27000);

      PAYOUTS.put(                          "Ammonia world", 48802);//48802
      PAYOUTS.put(                         "Earthlike body", (71874+65997)/2);//71874+65997
      PAYOUTS.put(                            "Water world", (32881+33112)/2);//32881+33112
      PAYOUTS.put(                            "Water giant", 1519);//1519
      PAYOUTS.put("High metal content body (Terraformable)", (50638+47257+41831+43757)/4);//50638+47257+41831+43757
      PAYOUTS.put(            "Water world (Terraformable)", (65490+63184+65222)/3);//65490+63184+65222

      PAYOUTS.put(      "Gas giant with ammonia based life", 1871);//1871
      PAYOUTS.put(        "Gas giant with water based life", 2063);//2063
      PAYOUTS.put(             "Sudarsky class I gas giant", (2613+4268)/2);//2613+4268
      PAYOUTS.put(            "Sudarsky class II gas giant", 12888);//12888
      PAYOUTS.put(           "Sudarsky class III gas giant", (1819+1393)/2);//1819+1393
      PAYOUTS.put(            "Sudarsky class IV gas giant", (2831+2748)/2);//2831+2748
      PAYOUTS.put(             "Sudarsky class V gas giant", 1865);//1865

      PAYOUTS.put(                "High metal content body", (6532+7567+5222)/3);//6532+7567+5222
      PAYOUTS.put(                               "Icy body", (925+1117+1155+902)/4);//925+1117+1155+902
      PAYOUTS.put(                        "Metal rich body", (12981+9241)/2);//12981+9241
      PAYOUTS.put(                             "Rocky body", (888+865)/2);//888+865
      PAYOUTS.put(                         "Rocky ice body", 1072);//1072
      //@formatter:on
    }

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

    private final LinkedList<VisitedSystem> visitedSystems = new LinkedList<>();
    private final Set<String> visitedSystemNames = new HashSet<>();

    private final List<TravelHistoryListener> listeners = new ArrayList<>();

    public TravelHistory(JournalReaderThread journalReaderThread, GameSession gameSession) {
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
                VisitedSystem visitedSystem = new VisitedSystem(e);
                if (visitedSystem.isUninhabited() && !this.visitedSystemNames.contains(visitedSystem.getSystemName())) {
                    if (visitedSystem.getCoord().distanceTo(new Coord(0, 0, 0)) > 200) {
                        visitedSystem.setRemainingPayout(PAYOUTS.getOrDefault("JUMP", 999999999));
                    }
                }
                this.visitedSystems.addLast(visitedSystem);
                this.visitedSystemNames.add(visitedSystem.getSystemName());
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
                this.setBodyName(e.getBodyName());
                this.setBodyType(ScanEntry.toBodyClass(e));
                ScannedBody scannedBody = new ScannedBody(e);
                scannedBody.setRemainingBasePayout(PAYOUTS.getOrDefault(scannedBody.getBodyClass(), 999999999));
                this.visitedSystems.getLast().getScannedBodies().add(scannedBody);
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
                                visitedSystem.setRemainingPayout(0);
                                payedOut = true;
                            }
                            for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                                scannedBody.setRemainingBasePayout(0);
                                scannedBody.setRemainingBonusPayout(0);
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
                        visitedSystem.setRemainingPayout(0);
                        for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
                            scannedBody.setRemainingBasePayout(0);
                            scannedBody.setRemainingBonusPayout(0);
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
        } else {
            this.setFuelCapacity(ship.getFuelCapacity());
            this.setFuelLevel(ship.getFuelCapacity() / 2.0f); // We don't know
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
        } else {
            this.setFuelCapacity(newShip.getFuelCapacity());
            this.setFuelLevel(newShip.getFuelCapacity() / 2.0f); // We don't know
        }
    }

}
