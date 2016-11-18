package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * RepairEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RepairEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5605338264840390395L;

    private final String item;
    private final Integer cost;

    public RepairEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.item = this.readString(data, "Item");
        this.cost = this.readInt(data, "Cost");
    }

    public String getItem() {
        return this.item;
    }

    public Integer getCost() {
        return this.cost;
    }

}
