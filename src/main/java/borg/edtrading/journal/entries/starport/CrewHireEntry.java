package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * CrewHireEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CrewHireEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 145345521296469986L;

    private final String name;
    private final String faction;
    private final Integer cost;
    private final Integer combatRank;

    public CrewHireEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
        this.faction = this.readString(data, "Faction");
        this.cost = this.readInt(data, "Cost");
        this.combatRank = this.readInt(data, "CombatRank");
    }

    public String getName() {
        return this.name;
    }

    public String getFaction() {
        return this.faction;
    }

    public Integer getCost() {
        return this.cost;
    }

    public Integer getCombatRank() {
        return this.combatRank;
    }

}
