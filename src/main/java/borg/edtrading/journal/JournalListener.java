package borg.edtrading.journal;

import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.sidepanel.ScannedBody;
import borg.edtrading.sidepanel.ShipLoadout;
import borg.edtrading.sidepanel.ShipModule;

/**
 * JournalListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface JournalListener {

    void onGameLoaded(String commander, String gameMode, String group, ShipLoadout currentShipLoadout);

    /**
     * General change in location. System jump, docked/undocked, supercruise entry/exit etc
     *
     * @param systemChanged
     *      Only <code>true</code> after a FSD jump
     */
    void onLocationChanged(boolean systemChanged);

    void onFuelLevelChanged(float currentFuelLevel);

    void onBodyScanned(ScannedBody scannedBody);

    void onExplorationDataSold(SellExplorationDataEntry e);

    void onShipModuleChanged(String slot, ShipModule oldModule, ShipModule newModule);

    void onShipChanged(ShipLoadout oldLoadout, ShipLoadout currentShipLoadout);

    void onInventoryReset(ItemType type, String name, int count);

    void onInventoryCollected(ItemType type, String name, int count);

    void onInventoryDiscarded(ItemType type, String name, int count);

    void onInventorySpent(ItemType type, String name, int count);

}
