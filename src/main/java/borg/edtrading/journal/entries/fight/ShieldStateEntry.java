package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ShieldStateEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShieldStateEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5229986910389691858L;

    private final Boolean shieldsUp;

    public ShieldStateEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.shieldsUp = this.readBoolean(data, "ShieldsUp");
    }

    public Boolean getShieldsUp() {
        return this.shieldsUp;
    }

}
