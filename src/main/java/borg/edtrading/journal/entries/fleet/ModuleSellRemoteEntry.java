package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ModuleSellRemoteEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ModuleSellRemoteEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5858765261595688642L;

    private final String slot;
    private final String sellItem;
    private final String sellItemLocalized;
    private final Integer sellPrice;
    private final String ship;
    private final Integer shipID;
    private final Integer serverId;
    private final Integer storageSlot;

    public ModuleSellRemoteEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.slot = this.readString(data, "Slot");
        this.sellItem = this.readString(data, "SellItem");
        this.sellItemLocalized = this.readString(data, "SellItem_Localised");
        this.sellPrice = this.readInt(data, "SellPrice");
        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
        this.serverId = this.readInt(data, "ServerId");
        this.storageSlot = this.readInt(data, "StorageSlot");
    }

    public String getSlot() {
        return this.slot;
    }

    public String getSellItem() {
        return this.sellItem;
    }

    public String getSellItemLocalized() {
        return this.sellItemLocalized;
    }

    public Integer getSellPrice() {
        return this.sellPrice;
    }

    public String getShip() {
        return this.ship;
    }

    public Integer getShipID() {
        return this.shipID;
    }

    public Integer getServerId() {
        return this.serverId;
    }

    public Integer getStorageSlot() {
        return this.storageSlot;
    }

}
