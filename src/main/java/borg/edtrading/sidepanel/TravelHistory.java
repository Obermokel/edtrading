package borg.edtrading.sidepanel;

import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * TravelHistory
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TravelHistory implements GameSessionListener, Serializable {

    private static final long serialVersionUID = 3845858336099915390L;

    static final Logger logger = LogManager.getLogger(TravelHistory.class);

    private transient ShipLoadout currentShip = null;

    private final Set<String> assumedFirstDiscoveries = new TreeSet<>();
    private final Set<String> visitedSystemNames = new HashSet<>();
    private final Set<String> scannedBodyNames = new HashSet<>();

    public TravelHistory(JournalReaderThread journalReaderThread, GameSession gameSession) {
        this.loadAssumedFirstDiscoveries();
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

    public boolean isVisited(String systemName) {
        return this.visitedSystemNames.contains(systemName);
    }

    public boolean isScanned(String bodyName) {
        return this.scannedBodyNames.contains(bodyName);
    }

    public LinkedList<VisitedSystem> getVisitedSystems() {
        //        return this.visitedSystems;
        return null;
    }

    public void setToAssumedFirstDiscovery(String bodyName) {
        //        if (bodyName != null) {
        //            if (this.assumedFirstDiscoveries.add(bodyName)) {
        //                this.saveAssumedFirstDiscoveries();
        //
        //                for (int i = this.visitedSystems.size() - 1; i >= 0; i--) {
        //                    VisitedSystem visitedSystem = this.visitedSystems.get(i);
        //                    for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
        //                        if (scannedBody.getBodyName().equals(bodyName)) {
        //                            scannedBody.setToFirstDiscovered();
        //                            for (TravelHistoryListener listener : this.listeners) {
        //                                try {
        //                                    listener.onLocationChanged();
        //                                } catch (Exception ex) {
        //                                    logger.warn(listener + " failed: " + ex);
        //                                }
        //                            }
        //                            return;
        //                        }
        //                    }
        //                }
        //            }
        //        }
    }

    public void unsetAssumedFirstDiscovery(String bodyName) {
        //        if (bodyName != null) {
        //            if (this.assumedFirstDiscoveries.remove(bodyName)) {
        //                this.saveAssumedFirstDiscoveries();
        //
        //                for (int i = this.visitedSystems.size() - 1; i >= 0; i--) {
        //                    VisitedSystem visitedSystem = this.visitedSystems.get(i);
        //                    for (ScannedBody scannedBody : visitedSystem.getScannedBodies()) {
        //                        if (scannedBody.getBodyName().equals(bodyName)) {
        //                            scannedBody.setToNotFirstDiscovered();
        //                            for (TravelHistoryListener listener : this.listeners) {
        //                                try {
        //                                    listener.onLocationChanged();
        //                                } catch (Exception ex) {
        //                                    logger.warn(listener + " failed: " + ex);
        //                                }
        //                            }
        //                            return;
        //                        }
        //                    }
        //                }
        //            }
        //        }
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

    private void onNewJournalEntry(AbstractJournalEntry entry) {

    }

    @Override
    public void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship) {
        //        if (ship == null) {
        //            this.setFuelCapacity(0);
        //            this.setFuelLevel(0);
        //            this.currentShip = null;
        //        } else {
        //            this.setFuelCapacity(ship.getFuelCapacity());
        //            this.setFuelLevel(MiscUtil.getAsFloat(ship.getFuelLevel(), 0f));
        //            this.currentShip = ship;
        //        }
    }

    @Override
    public void onShipModuleChanged(String slot, ShipModule oldModule, ShipModule newModule) {
        //        if (oldModule != null && oldModule.getFuelCapacity() != null) {
        //            this.setFuelCapacity(this.getFuelCapacity() - oldModule.getFuelCapacity());
        //        }
        //        if (newModule != null && newModule.getFuelCapacity() != null) {
        //            this.setFuelCapacity(this.getFuelCapacity() + newModule.getFuelCapacity());
        //        }
    }

    @Override
    public void onShipChanged(ShipLoadout oldShip, ShipLoadout newShip) {
        //        if (newShip == null) {
        //            this.setFuelCapacity(0);
        //            this.setFuelLevel(0);
        //            this.currentShip = null;
        //        } else {
        //            this.setFuelCapacity(newShip.getFuelCapacity());
        //            this.setFuelLevel(MiscUtil.getAsFloat(newShip.getFuelLevel(), 0f));
        //            this.currentShip = newShip;
        //        }
    }

}
