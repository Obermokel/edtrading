package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * SellDronesEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SellDronesEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 1812119874032278897L;

    private final String type;
    private final Integer count;
    private final Integer sellPrice;
    private final Integer totalSale;

    public SellDronesEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
        this.count = this.readInt(data, "Count");
        this.sellPrice = this.readInt(data, "SellPrice");
        this.totalSale = this.readInt(data, "TotalSale");
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

}
