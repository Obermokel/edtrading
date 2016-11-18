package borg.edtrading.journal.entries.location;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * USSDropEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class USSDropEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 6772553765100314379L;

    private final String ussType;
    private final String ussTypeLocalized;
    private final Integer ussThreat;

    public USSDropEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.ussType = this.readString(data, "USSType");
        this.ussTypeLocalized = this.readString(data, "USSType_Localised");
        this.ussThreat = this.readInt(data, "USSThreat");
    }

    public String getUssType() {
        return this.ussType;
    }

    public String getUssTypeLocalized() {
        return this.ussTypeLocalized;
    }

    public Integer getUssThreat() {
        return this.ussThreat;
    }

}
