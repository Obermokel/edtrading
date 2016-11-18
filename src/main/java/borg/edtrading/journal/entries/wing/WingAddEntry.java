package borg.edtrading.journal.entries.wing;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * WingAddEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class WingAddEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -3215950645442568425L;

    private final String name;

    public WingAddEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
    }

    public String getName() {
        return this.name;
    }

}
