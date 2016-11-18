package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ModuleSwapEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ModuleSwapEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5672566218110529394L;

    private final String fromSlot;
    private final String toSlot;
    private final String fromItem;
    private final String fromItemLocalized;
    private final String toItem;
    private final String toItemLocalized;
    private final String ship;
    private final Integer shipID;

    public ModuleSwapEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.fromSlot = this.readString(data, "FromSlot");
        this.toSlot = this.readString(data, "ToSlot");
        this.fromItem = this.readString(data, "FromItem");
        this.fromItemLocalized = this.readString(data, "FromItem_Localised");
        this.toItem = this.readString(data, "ToItem");
        this.toItemLocalized = this.readString(data, "ToItem_Localised");
        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
    }

    public String getFromSlot() {
        return this.fromSlot;
    }

    public String getToSlot() {
        return this.toSlot;
    }

    public String getFromItem() {
        return this.fromItem;
    }

    public String getFromItemLocalized() {
        return this.fromItemLocalized;
    }

    public String getToItem() {
        return this.toItem;
    }

    public String getToItemLocalized() {
        return this.toItemLocalized;
    }

    public String getShip() {
        return this.ship;
    }

    public Integer getShipID() {
        return this.shipID;
    }

}
