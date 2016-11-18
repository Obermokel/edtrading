package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * HullDamageEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class HullDamageEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -2654766196201111619L;

    private final Float health;

    public HullDamageEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.health = this.readFloat(data, "Health");
    }

    public Float getHealth() {
        return this.health;
    }

}
