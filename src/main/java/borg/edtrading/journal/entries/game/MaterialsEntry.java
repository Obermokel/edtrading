package borg.edtrading.journal.entries.game;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * MaterialsEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MaterialsEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 8108981946447113455L;

    private final Map<String, Integer> raw;
    private final Map<String, Integer> manufactured;
    private final Map<String, Integer> encoded;

    public MaterialsEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.raw = this.readNameCountMap(data, "Raw");
        this.manufactured = this.readNameCountMap(data, "Manufactured");
        this.encoded = this.readNameCountMap(data, "Encoded");
    }

    public Map<String, Integer> getRaw() {
        return this.raw;
    }

    public Map<String, Integer> getManufactured() {
        return this.manufactured;
    }

    public Map<String, Integer> getEncoded() {
        return this.encoded;
    }

}
