package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * RebootRepairEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RebootRepairEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -4817231892385082923L;

    private final List<String> modules;

    public RebootRepairEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.modules = this.readList(data, "Modules", String.class);
    }

    public List<String> getModules() {
        return this.modules;
    }

}
