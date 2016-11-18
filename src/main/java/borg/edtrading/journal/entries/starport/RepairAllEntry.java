package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * RepairAllEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RepairAllEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 3454130977574848114L;

    private final Integer cost;

    public RepairAllEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.cost = this.readInt(data, "Cost");
    }

    public Integer getCost() {
        return this.cost;
    }

}
