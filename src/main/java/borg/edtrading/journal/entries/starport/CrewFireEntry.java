package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * CrewFireEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CrewFireEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5193289069404961126L;

    private final String name;

    public CrewFireEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
    }

    public String getName() {
        return this.name;
    }

}
