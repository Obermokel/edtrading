package borg.edtrading.journal.entries.missions;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DataScannedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DataScannedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -8657482457745282236L;

    private final String type;

    public DataScannedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
    }

    public String getType() {
        return this.type;
    }

}
