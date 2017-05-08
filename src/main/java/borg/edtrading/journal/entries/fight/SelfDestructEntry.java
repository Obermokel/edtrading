package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * SelfDestructEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SelfDestructEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -4396887573302150523L;

    public SelfDestructEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);
        // TODO Auto-generated constructor stub
    }

}
