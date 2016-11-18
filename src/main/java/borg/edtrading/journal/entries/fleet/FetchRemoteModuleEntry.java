package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * FetchRemoteModuleEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FetchRemoteModuleEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 6449789146254671917L;

    private final Integer storageSlot;
    private final String storedItem;
    private final String storedItemLocalized;
    private final Integer serverId;
    private final Integer transferCost;
    private final String ship;
    private final Integer shipID;

    public FetchRemoteModuleEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.storageSlot = this.readInt(data, "StorageSlot");
        this.storedItem = this.readString(data, "StoredItem");
        this.storedItemLocalized = this.readString(data, "StoredItem_Localised");
        this.serverId = this.readInt(data, "ServerId");
        this.transferCost = this.readInt(data, "TransferCost");
        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
    }

    public Integer getStorageSlot() {
        return this.storageSlot;
    }

    public String getStoredItem() {
        return this.storedItem;
    }

    public String getStoredItemLocalized() {
        return this.storedItemLocalized;
    }

    public Integer getServerId() {
        return this.serverId;
    }

    public Integer getTransferCost() {
        return this.transferCost;
    }

    public String getShip() {
        return this.ship;
    }

    public Integer getShipID() {
        return this.shipID;
    }

}
