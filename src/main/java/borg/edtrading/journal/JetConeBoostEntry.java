package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * JetConeBoostEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class JetConeBoostEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 4656132100035871312L;

    private final Float boostValue;

    public JetConeBoostEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.boostValue = this.readFloat(data, "BoostValue");
    }

    public Float getBoostValue() {
        return this.boostValue;
    }

}
