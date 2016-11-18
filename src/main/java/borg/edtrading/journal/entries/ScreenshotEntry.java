package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * Took a screenshot
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScreenshotEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 6800303036947107178L;

    private final String filename;
    private final Integer width;
    private final Integer height;
    private final String system;
    private final String body;

    public ScreenshotEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.filename = this.readString(data, "Filename");
        this.width = this.readInt(data, "Width");
        this.height = this.readInt(data, "Height");
        this.system = this.readString(data, "System");
        this.body = this.readString(data, "Body");
    }

    public String getFilename() {
        return this.filename;
    }

    public Integer getWidth() {
        return this.width;
    }

    public Integer getHeight() {
        return this.height;
    }

    public String getSystem() {
        return this.system;
    }

    public String getBody() {
        return this.body;
    }

}
