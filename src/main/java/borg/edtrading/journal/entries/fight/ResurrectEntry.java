package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ResurrectEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ResurrectEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 8515408966104841526L;

    private final String option;
    private final Integer cost;
    private final Boolean bankrupt;

    public ResurrectEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.option = this.readString(data, "Option");
        this.cost = this.readInt(data, "Cost");
        this.bankrupt = this.readBoolean(data, "Bankrupt");
    }

    public String getOption() {
        return this.option;
    }

    public Integer getCost() {
        return this.cost;
    }

    public Boolean getBankrupt() {
        return this.bankrupt;
    }

}
