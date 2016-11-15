package borg.edtrading.sidepanel;

import borg.edtrading.data.Item.ItemType;

/**
 * InventoryListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface InventoryListener {

    void onInventoryReset(ItemType type, String name, int count);

    void onInventoryCollected(ItemType type, String name, int count);

    void onInventoryDiscarded(ItemType type, String name, int count);

    void onInventorySpent(ItemType type, String name, int count);

}
