package borg.edtrading.journal.entries.missions;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MissionFailedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MissionFailedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -265545630355337608L;

    private final String name;
    private final Integer missionID;

    public MissionFailedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
        this.missionID = this.readInt(data, "MissionID");
    }

    public String getName() {
        return this.name;
    }

    public Integer getMissionID() {
        return this.missionID;
    }

}
