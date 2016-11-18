package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DockingDeniedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DockingDeniedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 1515067881436573088L;

    private final String reason;
    private final String stationName;

    public DockingDeniedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.reason = this.readString(data, "Reason");
        this.stationName = this.readString(data, "StationName");
    }

    public String getReason() {
        return this.reason;
    }

    public String getStationName() {
        return this.stationName;
    }

}
