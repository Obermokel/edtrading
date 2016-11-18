package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * UndockedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class UndockedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5478676276968505735L;

    private final String stationName;
    private final String stationType;

    public UndockedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.stationName = this.readString(data, "StationName");
        this.stationType = this.readString(data, "StationType");
    }

    public String getStationName() {
        return this.stationName;
    }

    public String getStationType() {
        return this.stationType;
    }

}
