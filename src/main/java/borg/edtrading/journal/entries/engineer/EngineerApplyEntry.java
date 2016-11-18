package borg.edtrading.journal.entries.engineer;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * EngineerApplyEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EngineerApplyEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -8811965483015143162L;

    private final String engineer;
    private final String blueprint;
    private final Integer level;

    public EngineerApplyEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.engineer = this.readString(data, "Engineer");
        this.blueprint = this.readString(data, "Blueprint");
        this.level = this.readInt(data, "Level");
    }

    public String getEngineer() {
        return this.engineer;
    }

    public String getBlueprint() {
        return this.blueprint;
    }

    public Integer getLevel() {
        return this.level;
    }

}
