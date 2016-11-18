package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * RestockVehicleEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RestockVehicleEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -3549028101624173659L;

    private final String type;
    private final String loadout;
    private final Integer cost;
    private final Integer count;

    public RestockVehicleEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
        this.loadout = this.readString(data, "Loadout");
        this.cost = this.readInt(data, "Cost");
        this.count = this.readInt(data, "Count");
    }

    public String getType() {
        return this.type;
    }

    public String getLoadout() {
        return this.loadout;
    }

    public Integer getCost() {
        return this.cost;
    }

    public Integer getCount() {
        return this.count;
    }

}
