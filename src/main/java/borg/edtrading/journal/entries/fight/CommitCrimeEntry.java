package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * CommitCrimeEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CommitCrimeEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -8976591581501674824L;

    private final String crimeType;
    private final String faction;
    private final String victim;
    private final Integer bounty;
    private final Integer fine;

    public CommitCrimeEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.crimeType = this.readString(data, "CrimeType");
        this.faction = this.readString(data, "Faction");
        this.victim = this.readString(data, "Victim");
        this.bounty = this.readInt(data, "Bounty");
        this.fine = this.readInt(data, "Fine");
    }

    public String getCrimeType() {
        return this.crimeType;
    }

    public String getFaction() {
        return this.faction;
    }

    public String getVictim() {
        return this.victim;
    }

    public Integer getBounty() {
        return this.bounty;
    }

    public Integer getFine() {
        return this.fine;
    }

}
