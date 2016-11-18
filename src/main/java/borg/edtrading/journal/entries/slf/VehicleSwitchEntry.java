package borg.edtrading.journal.entries.slf;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * VehicleSwitchEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class VehicleSwitchEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5463400335529116439L;

    private final String to;

    public VehicleSwitchEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.to = this.readString(data, "To");
    }

    public String getTo() {
        return this.to;
    }

}
