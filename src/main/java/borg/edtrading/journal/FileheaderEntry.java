package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * FileheaderEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FileheaderEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 8244969083612523411L;

    private final Integer part;
    private final String language;
    private final String gameversion;
    private final String build;

    public FileheaderEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.part = this.readInt(data, "part");
        this.language = this.readString(data, "language");
        this.gameversion = this.readString(data, "gameversion");
        this.build = this.readString(data, "build");
    }

    public Integer getPart() {
        return this.part;
    }

    public String getLanguage() {
        return this.language;
    }

    public String getGameversion() {
        return this.gameversion;
    }

    public String getBuild() {
        return this.build;
    }

}
