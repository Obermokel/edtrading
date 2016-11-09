package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * DockFighterEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class DockFighterEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -5883877498131231560L;

    public DockFighterEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);
    }

}
