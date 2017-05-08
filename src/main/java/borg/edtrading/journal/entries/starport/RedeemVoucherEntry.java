package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * RedeemVoucherEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RedeemVoucherEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2283087097570972567L;

    private final String type;
    private final Integer amount;
    private final Integer brokerPercentage;
    private final String faction;
    private final Map<String, Integer> factions;

    public RedeemVoucherEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
        this.amount = this.readInt(data, "Amount");
        this.brokerPercentage = this.readInt(data, "BrokerPercentage");
        this.faction = this.readString(data, "Faction");
        this.factions = this.readAmounts(data, "Factions", "Faction");
    }

    public String getType() {
        return this.type;
    }

    public Integer getAmount() {
        return this.amount;
    }

    public Integer getBrokerPercentage() {
        return this.brokerPercentage;
    }

    public String getFaction() {
        return this.faction;
    }

    public Map<String, Integer> getFactions() {
        return this.factions;
    }

}
