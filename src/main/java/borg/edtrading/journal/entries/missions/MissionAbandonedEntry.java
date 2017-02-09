package borg.edtrading.journal.entries.missions;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MissionAbandonedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MissionAbandonedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 8326875187084791770L;

    private final Integer missionID;
    private final String name;

    public MissionAbandonedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.missionID = this.readInt(data, "MissionID");
        this.name = this.readString(data, "Name");
    }

    public Integer getMissionID() {
        return this.missionID;
    }

    public String getName() {
        return this.name;
    }

}
