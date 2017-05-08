package borg.edtrading.journal.entries.game;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * CargoEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CargoEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 1216712837075434298L;

    private final Map<String, Integer> inventory;

    public CargoEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.inventory = this.readNameCountMap(data, "Inventory");
    }

    public Map<String, Integer> getInventory() {
        return this.inventory;
    }

}
