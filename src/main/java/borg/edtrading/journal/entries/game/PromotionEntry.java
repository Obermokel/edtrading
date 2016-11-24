package borg.edtrading.journal.entries.game;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * PromotionEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PromotionEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 9058154050675300622L;

    public PromotionEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);
        // TODO Auto-generated constructor stub
    }

}
