package borg.edtrading.journal.entries.inventory;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ScientificResearchEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScientificResearchEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -4134653880613476387L;

    private final String name;
    private final String category;
    private final Integer count;

    public ScientificResearchEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
        this.category = this.readString(data, "Category");
        this.count = this.readInt(data, "Count");
    }

    public String getName() {
        return this.name;
    }

    public String getCategory() {
        return this.category;
    }

    public Integer getCount() {
        return this.count;
    }

}
