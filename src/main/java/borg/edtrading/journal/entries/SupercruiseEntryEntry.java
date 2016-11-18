package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * SupercruiseEntryEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SupercruiseEntryEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 6310633753334097859L;

    private final String starSystem;

    public SupercruiseEntryEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.starSystem = this.readString(data, "StarSystem");
    }

    public String getStarSystem() {
        return this.starSystem;
    }

}
