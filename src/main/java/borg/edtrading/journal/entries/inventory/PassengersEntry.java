package borg.edtrading.journal.entries.inventory;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.PassengerManifestData;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * PassengersEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PassengersEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 8573218534904039370L;

    private final List<PassengerManifestData> manifest;

    public PassengersEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.manifest = this.readPassengerManifest(data, "Manifest");
    }

    public List<PassengerManifestData> getManifest() {
        return this.manifest;
    }

}
