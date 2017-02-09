package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ShipyardSellEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipyardSellEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2521764792944638582L;

    private final Integer sellShipID;
    private final String shipType;
    private final Integer shipPrice;

    public ShipyardSellEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.sellShipID = this.readInt(data, "SellShipID");
        this.shipType = this.readString(data, "ShipType");
        this.shipPrice = this.readInt(data, "ShipPrice");
    }

    public Integer getSellShipID() {
        return this.sellShipID;
    }

    public String getShipType() {
        return this.shipType;
    }

    public Integer getShipPrice() {
        return this.shipPrice;
    }

}
