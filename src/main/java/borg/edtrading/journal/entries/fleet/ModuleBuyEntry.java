package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ModuleBuyEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ModuleBuyEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 4334974543669117289L;

    private final String slot;
    private final String buyItem;
    private final String buyItemLocalized;
    private final Integer buyPrice;
    private final String ship;
    private final Integer shipID;
    private final String sellItem;
    private final String sellItemLocalized;
    private final Integer sellPrice;
    private final String storedItem;
    private final String storedItemLocalized;

    public ModuleBuyEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.slot = this.readString(data, "Slot");
        this.buyItem = this.readString(data, "BuyItem");
        this.buyItemLocalized = this.readString(data, "BuyItem_Localised");
        this.buyPrice = this.readInt(data, "BuyPrice");
        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
        this.sellItem = this.readString(data, "SellItem");
        this.sellItemLocalized = this.readString(data, "SellItem_Localised");
        this.sellPrice = this.readInt(data, "SellPrice");
        this.storedItem = this.readString(data, "StoredItem");
        this.storedItemLocalized = this.readString(data, "StoredItem_Localised");
    }

    public String getSlot() {
        return this.slot;
    }

    public String getBuyItem() {
        return this.buyItem;
    }

    public String getBuyItemLocalized() {
        return this.buyItemLocalized;
    }

    public Integer getBuyPrice() {
        return this.buyPrice;
    }

    public String getShip() {
        return this.ship;
    }

    public Integer getShipID() {
        return this.shipID;
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

    public String getStoredItem() {
        return this.storedItem;
    }

    public String getStoredItemLocalized() {
        return this.storedItemLocalized;
    }

}
