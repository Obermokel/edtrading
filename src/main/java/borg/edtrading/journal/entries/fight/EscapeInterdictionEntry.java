package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * EscapeInterdictionEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EscapeInterdictionEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 1272334066800261276L;

    private final String interdictor;
    private final String interdictorLocalized;
    private final Boolean isPlayer;

    public EscapeInterdictionEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.interdictor = this.readString(data, "Interdictor");
        this.interdictorLocalized = this.readString(data, "Interdictor_Localised");
        this.isPlayer = this.readBoolean(data, "IsPlayer");
    }

    public String getInterdictor() {
        return this.interdictor;
    }

    public String getInterdictorLocalized() {
        return this.interdictorLocalized;
    }

    public Boolean getIsPlayer() {
        return this.isPlayer;
    }

}
