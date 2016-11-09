package borg.edtrading.journal;

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
    private final String message;

    public SendTextEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.to = this.readString(data, "To");
        this.message = this.readString(data, "Message");
    }

    public String getTo() {
        return this.to;
    }

    public String getMessage() {
        return this.message;
    }

}
