package borg.edtrading.journal.entries.location;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * TouchdownEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TouchdownEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5156592373026025455L;

    private final Float latitude;
    private final Float longitude;
    private final Boolean playerControlled;

    public TouchdownEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.latitude = this.readFloat(data, "Latitude");
        this.longitude = this.readFloat(data, "Longitude");
        this.playerControlled = this.readBoolean(data, "PlayerControlled");
    }

    public Float getLatitude() {
        return this.latitude;
    }

    public Float getLongitude() {
        return this.longitude;
    }

    public Boolean getPlayerControlled() {
        return this.playerControlled;
    }

}
