package borg.edtrading.journal.entries.inventory;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MaterialDiscoveredEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MaterialDiscoveredEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5289666233982265791L;

    private final String category;
    private final String name;
    private final Integer discoveryNumber;

    public MaterialDiscoveredEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.category = this.readString(data, "Category");
        this.name = this.readString(data, "Name");
        this.discoveryNumber = this.readInt(data, "DiscoveryNumber");
    }

    public String getCategory() {
        return this.category;
    }

    public String getName() {
        return this.name;
    }

    public Integer getDiscoveryNumber() {
        return this.discoveryNumber;
    }

}
