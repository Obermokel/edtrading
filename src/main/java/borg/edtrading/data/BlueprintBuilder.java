package borg.edtrading.data;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * BlueprintBuilder
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BlueprintBuilder {

    static final Logger logger = LogManager.getLogger(BlueprintBuilder.class);

    private Map<Item, Integer> items = new LinkedHashMap<>();

    public Map<Item, Integer> build() {
        return this.items;
    }

    public BlueprintBuilder add(Item item, int quantity) {
        Integer prev = this.items.get(item);
        if (prev == null) {
            this.items.put(item, quantity);
        } else {
            this.items.put(item, quantity + prev);
        }
        return this;
    }

}
