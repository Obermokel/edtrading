package borg.edtrading.journal.entries.missions;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * CommunityGoalJoinEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CommunityGoalJoinEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 4797026824583778460L;

    private final String name;
    private final String system;

    public CommunityGoalJoinEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
        this.system = this.readString(data, "System");
    }

    public String getName() {
        return this.name;
    }

    public String getSystem() {
        return this.system;
    }

}
