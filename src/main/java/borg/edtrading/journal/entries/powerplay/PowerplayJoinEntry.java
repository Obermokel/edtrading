package borg.edtrading.journal.entries.powerplay;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * PowerplayJoinEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PowerplayJoinEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 8820466751584100018L;

    private final String power;

    public PowerplayJoinEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.power = this.readString(data, "Power");
    }

    public String getPower() {
        return this.power;
    }

}
