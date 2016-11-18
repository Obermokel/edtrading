package borg.edtrading.journal.entries.comms;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * SendTextEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SendTextEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -6024903378452865704L;

    private final String to;
    private final String toLocalized;
    private final String message;

    public SendTextEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.to = this.readString(data, "To");
        this.toLocalized = this.readString(data, "To_Localised");
        this.message = this.readString(data, "Message");
    }

    public String getTo() {
        return this.to;
    }

    public String getToLocalized() {
        return this.toLocalized;
    }

    public String getMessage() {
        return this.message;
    }

}
