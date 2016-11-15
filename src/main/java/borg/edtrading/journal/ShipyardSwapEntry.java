package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ShipyardSwapEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipyardSwapEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 7598652203788686038L;

    private final String shipType;
    private final Integer shipID;
    private final String storeOldShip;
    private final Integer storeShipID;

    public ShipyardSwapEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.shipType = this.readString(data, "ShipType");
        this.shipID = this.readInt(data, "ShipID");
        this.storeOldShip = this.readString(data, "StoreOldShip");
        this.storeShipID = this.readInt(data, "StoreShipID");
    }

    public String getShipType() {
        return this.shipType;
    }

    public Integer getShipID() {
        return this.shipID;
    }

    public String getStoreOldShip() {
        return this.storeOldShip;
    }

    public Integer getStoreShipID() {
        return this.storeShipID;
    }

}
