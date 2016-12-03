package borg.edtrading.sidepanel;

import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;

/**
 * TravelHistoryListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface TravelHistoryListener {

    void onSystemChanged();

    void onLocationChanged();

    void onBodyScanned(ScannedBody scannedBody);

    void onFuelLevelChanged(float newFuelLevel);

    void onExplorationDataSold(SellExplorationDataEntry journalEntry);

}
