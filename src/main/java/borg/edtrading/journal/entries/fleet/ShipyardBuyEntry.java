package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

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
    private final String sellOldShip;
    private final Integer sellShipID;
    private final Integer sellPrice;

    public ShipyardBuyEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.shipType = this.readString(data, "ShipType");
        this.shipPrice = this.readInt(data, "ShipPrice");
        this.storeOldShip = this.readString(data, "StoreOldShip");
        this.storeShipID = this.readInt(data, "StoreShipID");
        this.sellOldShip = this.readString(data, "SellOldShip");
        this.sellShipID = this.readInt(data, "SellShipID");
        this.sellPrice = this.readInt(data, "SellPrice");
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

    public String getSellOldShip() {
        return this.sellOldShip;
    }

    public Integer getSellShipID() {
        return this.sellShipID;
    }

    public Integer getSellPrice() {
        return this.sellPrice;
    }

}
