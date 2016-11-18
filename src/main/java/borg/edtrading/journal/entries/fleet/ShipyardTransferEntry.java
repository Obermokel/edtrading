package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ShipyardTransferEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipyardTransferEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2109255635381365004L;

    private final String shipType;
    private final Integer shipID;
    private final String system;
    private final Float distance;
    private final Integer transferPrice;

    public ShipyardTransferEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.shipType = this.readString(data, "ShipType");
        this.shipID = this.readInt(data, "ShipID");
        this.system = this.readString(data, "System");
        this.distance = this.readFloat(data, "Distance");
        this.transferPrice = this.readInt(data, "TransferPrice");
    }

    public String getShipType() {
        return this.shipType;
    }

    public Integer getShipID() {
        return this.shipID;
    }

    public String getSystem() {
        return this.system;
    }

    public Float getDistance() {
        return this.distance;
    }

    public Integer getTransferPrice() {
        return this.transferPrice;
    }

}
