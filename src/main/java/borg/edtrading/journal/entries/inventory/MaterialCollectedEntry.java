package borg.edtrading.journal.entries.inventory;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MaterialCollectedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MaterialCollectedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 3372218657370815819L;

    private final String category;
    private final String name;
    private final Integer count;

    public MaterialCollectedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.category = this.readString(data, "Category");
        this.name = this.readString(data, "Name");
        this.count = this.readInt(data, "Count");
    }

    public String getCategory() {
        return this.category;
    }

    public String getName() {
        return this.name;
    }

    public Integer getCount() {
        return this.count;
    }

}
