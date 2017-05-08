package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * PVPKillEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PVPKillEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 7472706879123845211L;

    private final String victim;
    private final Integer combatRank;

    public PVPKillEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.victim = this.readString(data, "Victim");
        this.combatRank = this.readInt(data, "CombatRank");
    }

    public String getVictim() {
        return this.victim;
    }

    public Integer getCombatRank() {
        return this.combatRank;
    }

}
