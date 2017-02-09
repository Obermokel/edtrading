package borg.edtrading.journal.entries.srv;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DatalinkVoucherEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DatalinkVoucherEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -4573013355053074752L;

    private final Integer reward;
    private final String victimFaction;
    private final String payeeFaction;

    public DatalinkVoucherEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.reward = this.readInt(data, "Reward");
        this.victimFaction = this.readString(data, "VictimFaction");
        this.payeeFaction = this.readString(data, "PayeeFaction");
    }

    public Integer getReward() {
        return this.reward;
    }

    public String getVictimFaction() {
        return this.victimFaction;
    }

    public String getPayeeFaction() {
        return this.payeeFaction;
    }

}
