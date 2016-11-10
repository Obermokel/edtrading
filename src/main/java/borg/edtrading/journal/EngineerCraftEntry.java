package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * EngineerCraftEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EngineerCraftEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5502695653270755824L;

    private final String engineer;
    private final String blueprint;
    private final Integer level;
    private final Map<String, Integer> ingredients;

    public EngineerCraftEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.engineer = this.readString(data, "Engineer");
        this.blueprint = this.readString(data, "Blueprint");
        this.level = this.readInt(data, "Level");
        this.ingredients = this.readNameCountMap(data, "Ingredients");
    }

    public String getEngineer() {
        return this.engineer;
    }

    public String getBlueprint() {
        return this.blueprint;
    }

    public Integer getLevel() {
        return this.level;
    }

    public Map<String, Integer> getIngredients() {
        return this.ingredients;
    }

}
