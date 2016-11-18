package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MaterialDiscardedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MaterialDiscardedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -8061724964621377393L;

    private final String category;
    private final String name;
    private final Integer count;

    public MaterialDiscardedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
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
