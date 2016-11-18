package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * Refueled at station
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RefuelAllEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -1249495578563161608L;

    private final Integer cost;
    private final Float amount;

    public RefuelAllEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.cost = this.readInt(data, "Cost");
        this.amount = this.readFloat(data, "Amount");
    }

    public Integer getCost() {
        return this.cost;
    }

    public Float getAmount() {
        return this.amount;
    }

}
