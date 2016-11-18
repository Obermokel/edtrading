package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ModuleSellEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ModuleSellEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 6169068487031852671L;

    private final String slot;
    private final String sellItem;
    private final String sellItemLocalized;
    private final Integer sellPrice;
    private final String ship;
    private final Integer shipID;

    public ModuleSellEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.slot = this.readString(data, "Slot");
        this.sellItem = this.readString(data, "SellItem");
        this.sellItemLocalized = this.readString(data, "SellItem_Localised");
        this.sellPrice = this.readInt(data, "SellPrice");
        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
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

}
