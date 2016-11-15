package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ShipyardBuyEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipyardBuyEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -1184275777763249270L;

    private final String shipType;
    private final Integer shipPrice;
    private final String storeOldShip;
    private final Integer storeShipID;

    public ShipyardBuyEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.shipType = this.readString(data, "ShipType");
        this.shipPrice = this.readInt(data, "ShipPrice");
        this.storeOldShip = this.readString(data, "StoreOldShip");
        this.storeShipID = this.readInt(data, "StoreShipID");
    }

    public String getShipType() {
        return this.shipType;
    }

    public Integer getShipPrice() {
        return this.shipPrice;
    }

    public String getStoreOldShip() {
        return this.storeOldShip;
    }

    public Integer getStoreShipID() {
        return this.storeShipID;
    }

}
