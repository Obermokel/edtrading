package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * BuyDronesEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BuyDronesEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -3570358766898381941L;

    private final String type;
    private final Integer count;
    private final Integer buyPrice;
    private final Integer totalCost;

    public BuyDronesEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
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
