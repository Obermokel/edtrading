package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * SupercruiseExitEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SupercruiseExitEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5874678375689564995L;

    private final String starSystem;
    private final String body;
    private final String bodyType;

    public SupercruiseExitEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.starSystem = this.readString(data, "StarSystem");
        this.body = this.readString(data, "Body");
        this.bodyType = this.readString(data, "BodyType");
    }

    public String getStarSystem() {
        return this.starSystem;
    }

    public String getBody() {
        return this.body;
    }

    public String getBodyType() {
        return this.bodyType;
    }

}
