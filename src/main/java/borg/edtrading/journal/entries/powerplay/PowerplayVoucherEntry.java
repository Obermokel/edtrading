package borg.edtrading.journal.entries.powerplay;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * PowerplayVoucherEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PowerplayVoucherEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5012264588585547866L;

    private final String power;
    private final List<String> systems;

    public PowerplayVoucherEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.power = this.readString(data, "Power");
        this.systems = this.readList(data, "Systems", String.class);
    }

    public String getPower() {
        return this.power;
    }

    public List<String> getSystems() {
        return this.systems;
    }

}
