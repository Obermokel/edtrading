package borg.edtrading.journal.entries.fleet;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ShipyardNewEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipyardNewEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 1934491923284771738L;

    private final String shipType;
    private final Integer newShipID;

    public ShipyardNewEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.shipType = this.readString(data, "ShipType");
        this.newShipID = this.readInt(data, "NewShipID");
    }

    public String getShipType() {
        return this.shipType;
    }

    public Integer getNewShipID() {
        return this.newShipID;
    }

}
