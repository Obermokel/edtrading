package borg.edtrading.journal.entries.engineer;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * EngineerProgressEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EngineerProgressEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -9113302947356310986L;

    private final String engineer;
    private final String progress;
    private final Integer rank;

    public EngineerProgressEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.engineer = this.readString(data, "Engineer");
        this.progress = this.readString(data, "Progress");
        this.rank = this.readInt(data, "Rank");
    }

    public String getEngineer() {
        return this.engineer;
    }

    public String getProgress() {
        return this.progress;
    }

    public Integer getRank() {
        return this.rank;
    }

}
