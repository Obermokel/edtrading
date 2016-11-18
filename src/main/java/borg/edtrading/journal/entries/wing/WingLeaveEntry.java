package borg.edtrading.journal.entries.wing;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * WingLeaveEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class WingLeaveEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -1115320073822165773L;

    public WingLeaveEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);
        // TODO Auto-generated constructor stub
    }

}
