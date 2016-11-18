package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * CollectCargoEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CollectCargoEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -350286762279221386L;

    private final String type;
    private final Boolean stolen;

    public CollectCargoEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
        this.stolen = this.readBoolean(data, "Stolen");
    }

    public String getType() {
        return this.type;
    }

    public Boolean getStolen() {
        return this.stolen;
    }

}
