package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MarketBuyEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MarketBuyEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 6535738820336571056L;

    private final String type;
    private final Integer count;
    private final Integer buyPrice;
    private final Integer totalCost;

    public MarketBuyEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
        this.count = this.readInt(data, "Count");
        this.buyPrice = this.readInt(data, "BuyPrice");
        this.totalCost = this.readInt(data, "TotalCost");
    }

    public String getType() {
        return this.type;
    }

    public Integer getCount() {
        return this.count;
    }

    public Integer getBuyPrice() {
        return this.buyPrice;
    }

    public Integer getTotalCost() {
        return this.totalCost;
    }

}
