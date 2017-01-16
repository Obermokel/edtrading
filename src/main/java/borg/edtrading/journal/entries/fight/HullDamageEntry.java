package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * HullDamageEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class HullDamageEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -2654766196201111619L;

    private final Float health;
    private final Boolean playerPilot;

    public HullDamageEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.health = this.readFloat(data, "Health");
        this.playerPilot = this.readBoolean(data, "PlayerPilot");
    }

    public Float getHealth() {
        return this.health;
    }

    public Boolean getPlayerPilot() {
        return this.playerPilot;
    }

}
