package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * InterdictedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InterdictedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5449229659370235579L;

    private final Boolean submitted;
    private final String interdictor;
    private final String interdictorLocalized;
    private final Boolean isPlayer;
    private final String faction;
    private final String power;
    private final Integer combatRank;

    public InterdictedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.submitted = this.readBoolean(data, "Submitted");
        this.interdictor = this.readString(data, "Interdictor");
        this.interdictorLocalized = this.readString(data, "Interdictor_Localised");
        this.isPlayer = this.readBoolean(data, "IsPlayer");
        this.faction = this.readString(data, "Faction");
        this.power = this.readString(data, "Power");
        this.combatRank = this.readInt(data, "CombatRank");
    }

    public Boolean getSubmitted() {
        return this.submitted;
    }

    public String getInterdictor() {
        return this.interdictor;
    }

    public String getInterdictorLocalized() {
        return this.interdictorLocalized;
    }

    public Boolean getIsPlayer() {
        return this.isPlayer;
    }

    public String getFaction() {
        return this.faction;
    }

    public String getPower() {
        return this.power;
    }

    public Integer getCombatRank() {
        return this.combatRank;
    }

}
