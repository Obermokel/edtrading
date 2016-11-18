package borg.edtrading.journal.entries.location;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ApproachSettlementEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ApproachSettlementEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -1003106361414263540L;

    private final String name;

    public ApproachSettlementEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
    }

    public String getName() {
        return this.name;
    }

}
