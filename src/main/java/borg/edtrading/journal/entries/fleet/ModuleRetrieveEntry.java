package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ModuleRetrieveEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ModuleRetrieveEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -1138517248281791226L;

    private final String slot;
    private final String retrievedItem;
    private final String retrievedItemLocalized;
    private final String ship;
    private final Integer shipID;
    private final String swapOutItem;
    private final String swapOutItemLocalized;
    private final Integer cost;
    private final String engineerModifications;

    public ModuleRetrieveEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.slot = this.readString(data, "Slot");
        this.retrievedItem = this.readString(data, "RetrievedItem");
        this.retrievedItemLocalized = this.readString(data, "RetrievedItem_Localised");
        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
        this.swapOutItem = this.readString(data, "SwapOutItem");
        this.swapOutItemLocalized = this.readString(data, "SwapOutItem_Localised");
        this.cost = this.readInt(data, "Cost");
        this.engineerModifications = this.readString(data, "EngineerModifications");
    }

    public String getSlot() {
        return this.slot;
    }

    public String getRetrievedItem() {
        return this.retrievedItem;
    }

    public String getRetrievedItemLocalized() {
        return this.retrievedItemLocalized;
    }

    public String getShip() {
        return this.ship;
    }

    public Integer getShipID() {
        return this.shipID;
    }

    public String getSwapOutItem() {
        return this.swapOutItem;
    }

    public String getSwapOutItemLocalized() {
        return this.swapOutItemLocalized;
    }

    public Integer getCost() {
        return this.cost;
    }

    public String getEngineerModifications() {
        return this.engineerModifications;
    }

}
