package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MiningRefinedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MiningRefinedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -2358219833190479198L;

    private final String type;
    private final String typeLocalized;

    public MiningRefinedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.type = this.readString(data, "Type");
        this.typeLocalized = this.readString(data, "Type_Localised");
    }

    public String getType() {
        return this.type;
    }

    public String getTypeLocalized() {
        return this.typeLocalized;
    }

}
