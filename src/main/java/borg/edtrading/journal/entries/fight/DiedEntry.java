package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DiedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DiedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -3013695792045198481L;

    private final String killerName;
    private final String killerNameLocalized;
    private final String killerShip;
    private final String killerRank;

    public DiedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.killerName = this.readString(data, "KillerName");
        this.killerNameLocalized = this.readString(data, "KillerName_Localised");
        this.killerShip = this.readString(data, "KillerShip");
        this.killerRank = this.readString(data, "KillerRank");
    }

    public String getKillerName() {
        return this.killerName;
    }

    public String getKillerNameLocalized() {
        return this.killerNameLocalized;
    }

    public String getKillerShip() {
        return this.killerShip;
    }

    public String getKillerRank() {
        return this.killerRank;
    }

}
