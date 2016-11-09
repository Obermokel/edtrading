package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * SellExplorationDataEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SellExplorationDataEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2972277640104019388L;

    private final List<String> systems;
    private final List<String> discovered;
    private final Integer baseValue;
    private final Integer bonus;

    public SellExplorationDataEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.systems = this.readList(data, "Systems", String.class);
        this.discovered = this.readList(data, "Discovered", String.class);
        this.baseValue = this.readInt(data, "BaseValue");
        this.bonus = this.readInt(data, "Bonus");
    }

    public List<String> getSystems() {
        return this.systems;
    }

    public List<String> getDiscovered() {
        return this.discovered;
    }

    public Integer getBaseValue() {
        return this.baseValue;
    }

    public Integer getBonus() {
        return this.bonus;
    }

}
