package borg.edtrading.journal.entries.game;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * ProgressEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ProgressEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -2720106312862604219L;

    private final Integer combat;
    private final Integer trade;
    private final Integer explore;
    private final Integer empire;
    private final Integer federation;
    private final Integer cqc;

    public ProgressEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.combat = this.readInt(data, "Combat");
        this.trade = this.readInt(data, "Trade");
        this.explore = this.readInt(data, "Explore");
        this.empire = this.readInt(data, "Empire");
        this.federation = this.readInt(data, "Federation");
        this.cqc = this.readInt(data, "CQC");
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

    public Integer getEmpire() {
        return this.empire;
    }

    public Integer getFederation() {
        return this.federation;
    }

    public Integer getCqc() {
        return this.cqc;
    }

}
