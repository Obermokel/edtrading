package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * PayFinesEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PayFinesEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2574608223235643079L;

    private final Integer amount;
    private final Integer brokerPercentage;

    public PayFinesEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.amount = this.readInt(data, "Amount");
        this.brokerPercentage = this.readInt(data, "BrokerPercentage");
    }

    public Integer getAmount() {
        return this.amount;
    }

    public Integer getBrokerPercentage() {
        return this.brokerPercentage;
    }

}
