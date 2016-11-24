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

    private final Integer combat;
    private final Integer trade;
    private final Integer explore;

    public PromotionEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.combat = this.readInt(data, "Combat");
        this.trade = this.readInt(data, "Trade");
        this.explore = this.readInt(data, "Explore");
    }

    public Integer getCombat() {
        return this.combat;
    }

    public Integer getTrade() {
        return this.trade;
    }

    public Integer getExplore() {
        return this.explore;
    }

}
