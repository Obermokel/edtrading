package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ReceiveTextEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ReceiveTextEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -3055475950392754418L;

    private final String from;
    private final String fromLocalized;
    private final String message;
    private final String messageLocalized;
    private final String channel;

    public ReceiveTextEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.from = this.readString(data, "From");
        this.fromLocalized = this.readString(data, "From_Localised");
        this.message = this.readString(data, "Message");
        this.messageLocalized = this.readString(data, "Message_Localised");
        this.channel = this.readString(data, "Channel");
    }

    public String getFrom() {
        return this.from;
    }

    public String getFromLocalized() {
        return this.fromLocalized;
    }

    public String getMessage() {
        return this.message;
    }

    public String getMessageLocalized() {
        return this.messageLocalized;
    }

    public String getChannel() {
        return this.channel;
    }

}
