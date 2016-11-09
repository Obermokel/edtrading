package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DockingRequestedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DockingRequestedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -4566081435245301064L;

    private final String stationName;

    public DockingRequestedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.stationName = this.readString(data, "StationName");
    }

    public String getStationName() {
        return this.stationName;
    }

}
