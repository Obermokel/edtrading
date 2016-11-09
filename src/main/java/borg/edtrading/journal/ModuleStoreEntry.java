package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ModuleStoreEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ModuleStoreEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5866020171897010726L;

    private final String slot;
    private final String storedItem;
    private final String storedItemLocalized;
    private final String ship;
    private final Integer shipID;
    private final String engineerModifications;

    public ModuleStoreEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.slot = this.readString(data, "Slot");
        this.storedItem = this.readString(data, "StoredItem");
        this.storedItemLocalized = this.readString(data, "StoredItem_Localised");
        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
        this.engineerModifications = this.readString(data, "EngineerModifications");
    }

    public String getSlot() {
        return this.slot;
    }

    public String getStoredItem() {
        return this.storedItem;
    }

    public String getStoredItemLocalized() {
        return this.storedItemLocalized;
    }

    public String getShip() {
        return this.ship;
    }

    public Integer getShipID() {
        return this.shipID;
    }

    public String getEngineerModifications() {
        return this.engineerModifications;
    }

}
