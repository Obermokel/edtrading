package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DockSRVEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DockSRVEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -1758127977080765807L;

    public DockSRVEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);
    }

}
