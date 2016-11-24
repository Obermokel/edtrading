package borg.edtrading.journal.entries.srv;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DatalinkScanEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DatalinkScanEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -566260408261536013L;

    public DatalinkScanEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);
        // TODO Auto-generated constructor stub
    }

}
