package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * LaunchFighterEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class LaunchFighterEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -3046366029838915354L;

    private final String loadout;
    private final Boolean playerControlled;

    public LaunchFighterEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
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
