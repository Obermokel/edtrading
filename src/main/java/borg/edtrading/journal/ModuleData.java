package borg.edtrading.journal;

import java.io.Serializable;

/**
 * ModuleData
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ModuleData implements Serializable {

    private static final long serialVersionUID = 8482750169781894598L;

    private final String slot;
    private final String item;
    private final Boolean on;
    private final Integer priority;
    private final Float health;
    private final Integer value;

    public ModuleData(String slot, String item, Boolean on, Integer priority, Float health, Integer value) {
        this.slot = slot;
        this.item = item;
        this.on = on;
        this.priority = priority;
        this.health = health;
        this.value = value;
    }

    public String getSlot() {
        return this.slot;
    }

    public String getItem() {
        return this.item;
    }

    public Boolean getOn() {
        return this.on;
    }

    public Integer getPriority() {
        return this.priority;
    }

    public Float getHealth() {
        return this.health;
    }

    public Integer getValue() {
        return this.value;
    }

}
