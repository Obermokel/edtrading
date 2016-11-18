package borg.edtrading.journal.entries.travel;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * JetConeDamageEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class JetConeDamageEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 2215190214002675083L;

    private final String module;
    private final String moduleLocalized;

    public JetConeDamageEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.module = this.readString(data, "Module");
        this.moduleLocalized = this.readString(data, "Module_Localised");
    }

    public String getModule() {
        return this.module;
    }

    public String getModuleLocalized() {
        return this.moduleLocalized;
    }

}
