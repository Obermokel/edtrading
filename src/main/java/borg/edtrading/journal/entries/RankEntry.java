package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * RankEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RankEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 1369004277749340776L;

    private final Integer combat;
    private final Integer trade;
    private final Integer explore;
    private final Integer empire;
    private final Integer federation;
    private final Integer cqc;

    public RankEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
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
