package borg.edtrading.journal.entries.powerplay;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * PowerplayLeaveEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PowerplayLeaveEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -6676623611182774790L;

    private final String power;

    public PowerplayLeaveEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.power = this.readString(data, "Power");
    }

    public String getPower() {
        return this.power;
    }

}
