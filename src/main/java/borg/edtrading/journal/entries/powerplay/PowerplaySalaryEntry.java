package borg.edtrading.journal.entries.powerplay;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * PowerplaySalaryEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PowerplaySalaryEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -184647996639014234L;

    private final String power;
    private final Integer amount;

    public PowerplaySalaryEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.power = this.readString(data, "Power");
        this.amount = this.readInt(data, "Amount");
    }

    public String getPower() {
        return this.power;
    }

    public Integer getAmount() {
        return this.amount;
    }

}
