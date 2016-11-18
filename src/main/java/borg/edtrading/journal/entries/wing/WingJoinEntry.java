package borg.edtrading.journal.entries.wing;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * WingJoinEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class WingJoinEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -6099786351212515422L;

    private final List<String> others;

    public WingJoinEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.others = this.readList(data, "Others", String.class);
    }

    public List<String> getOthers() {
        return this.others;
    }

}
