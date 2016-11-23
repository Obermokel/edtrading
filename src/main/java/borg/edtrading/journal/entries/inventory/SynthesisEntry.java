package borg.edtrading.journal.entries.inventory;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * SynthesisEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SynthesisEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 5472041684094836346L;

    private final String name;
    private final Map<String, Integer> materials;

    public SynthesisEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.name = this.readString(data, "Name");
        this.materials = this.readNameCountMap(data, "Materials");
    }

    public String getName() {
        return this.name;
    }

    public Map<String, Integer> getMaterials() {
        return this.materials;
    }

}
