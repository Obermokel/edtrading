package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * LaunchSRVEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class LaunchSRVEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 4217393168334459404L;

    private final String loadout;
    private final Boolean playerControlled;

    public LaunchSRVEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.loadout = this.readString(data, "Loadout");
        this.playerControlled = this.readBoolean(data, "PlayerControlled");
    }

    public String getLoadout() {
        return this.loadout;
    }

    public Boolean getPlayerControlled() {
        return this.playerControlled;
    }

}
