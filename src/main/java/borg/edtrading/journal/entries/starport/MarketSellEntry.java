package borg.edtrading.journal.entries.starport;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MarketSellEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MarketSellEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 3738239110891847865L;

    private final String type;
    private final Integer count;
    private final Integer sellPrice;
    private final Integer totalSale;
    private final Integer avgPricePaid;
    private final Boolean stolenGoods;
    private final Boolean blackMarket;

    public MarketSellEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
        this.count = this.readInt(data, "Count");
        this.sellPrice = this.readInt(data, "SellPrice");
        this.totalSale = this.readInt(data, "TotalSale");
        this.avgPricePaid = this.readInt(data, "AvgPricePaid");
        this.stolenGoods = this.readBoolean(data, "StolenGoods");
        this.blackMarket = this.readBoolean(data, "BlackMarket");
    }

    public String getType() {
        return this.type;
    }

    public Integer getCount() {
        return this.count;
    }

    public Integer getSellPrice() {
        return this.sellPrice;
    }

    public Integer getTotalSale() {
        return this.totalSale;
    }

    public Integer getAvgPricePaid() {
        return this.avgPricePaid;
    }

    public Boolean getStolenGoods() {
        return this.stolenGoods;
    }

    public Boolean getBlackMarket() {
        return this.blackMarket;
    }

}
