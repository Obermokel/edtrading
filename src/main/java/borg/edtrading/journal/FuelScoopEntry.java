package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * FuelScoopEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FuelScoopEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 156036021052185627L;

    private final Float scooped;
    private final Float total;

    public FuelScoopEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.scooped = this.readFloat(data, "Scooped");
        this.total = this.readFloat(data, "Total");
    }

    public Float getScooped() {
        return this.scooped;
    }

    public Float getTotal() {
        return this.total;
    }

}
