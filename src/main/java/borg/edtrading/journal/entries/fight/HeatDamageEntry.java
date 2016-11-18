package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * HeatDamageEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class HeatDamageEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2376501977506357736L;

    public HeatDamageEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);
        // TODO Auto-generated constructor stub
    }

}
