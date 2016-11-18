package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * EjectCargoEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EjectCargoEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2364187534996319959L;

    private final String type;
    private final Integer count;
    private final Boolean abandoned;

    public EjectCargoEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
        this.count = this.readInt(data, "Count");
        this.abandoned = this.readBoolean(data, "Abandoned");
    }

    public String getType() {
        return this.type;
    }

    public Integer getCount() {
        return this.count;
    }

    public Boolean getAbandoned() {
        return this.abandoned;
    }

}
