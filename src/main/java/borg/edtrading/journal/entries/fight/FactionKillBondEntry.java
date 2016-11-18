package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * FactionKillBondEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FactionKillBondEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 1003662069391320987L;

    private final String victimFaction;
    private final String awardingFaction;
    private final Integer reward;

    public FactionKillBondEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.victimFaction = this.readString(data, "VictimFaction");
        this.awardingFaction = this.readString(data, "AwardingFaction");
        this.reward = this.readInt(data, "Reward");
    }

    public String getVictimFaction() {
        return this.victimFaction;
    }

    public String getAwardingFaction() {
        return this.awardingFaction;
    }

    public Integer getReward() {
        return this.reward;
    }

}
