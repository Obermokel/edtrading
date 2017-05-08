package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ScannedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScannedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 782938807614617815L;

    private final String scanType;

    public ScannedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.scanType = this.readString(data, "ScanType");
    }

    public String getScanType() {
        return this.scanType;
    }

}
