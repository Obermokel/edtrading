package borg.edtrading.journal.entries.missions;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * CommunityGoalRewardEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CommunityGoalRewardEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -8698642979930165789L;

    private final String name;
    private final String system;
    private final Integer reward;

    public CommunityGoalRewardEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
        this.system = this.readString(data, "System");
        this.reward = this.readInt(data, "Reward");
    }

    public String getName() {
        return this.name;
    }

    public String getSystem() {
        return this.system;
    }

    public Integer getReward() {
        return this.reward;
    }

}
