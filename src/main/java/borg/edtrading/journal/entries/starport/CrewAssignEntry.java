package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * CrewAssignEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CrewAssignEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -960129223932775345L;

    private final String name;
    private final String role;

    public CrewAssignEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
        this.role = this.readString(data, "Role");
    }

    public String getName() {
        return this.name;
    }

    public String getRole() {
        return this.role;
    }

}
