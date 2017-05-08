package borg.edtrading.journal.entries.location;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * StartJumpEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class StartJumpEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5635262331882022926L;

    private final String jumpType;
    private final String starSystem;
    private final String starClass;

    public StartJumpEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.jumpType = this.readString(data, "JumpType");
        this.starSystem = this.readString(data, "StarSystem");
        this.starClass = this.readString(data, "StarClass");
    }

    public String getJumpType() {
        return this.jumpType;
    }

    public String getStarSystem() {
        return this.starSystem;
    }

    public String getStarClass() {
        return this.starClass;
    }

}
