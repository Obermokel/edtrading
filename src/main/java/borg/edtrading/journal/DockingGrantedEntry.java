package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DockingGrantedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DockingGrantedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -1153536107567299713L;

    private final Integer landingPad;
    private final String stationName;

    public DockingGrantedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.landingPad = this.readInt(data, "LandingPad");
        this.stationName = this.readString(data, "StationName");
    }

    public Integer getLandingPad() {
        return this.landingPad;
    }

    public String getStationName() {
        return this.stationName;
    }

}
