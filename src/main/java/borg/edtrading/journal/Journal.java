package borg.edtrading.journal;

import borg.edtrading.journal.entries.AbstractJournalEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

/**
 * Journal
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Journal {

    static final Logger logger = LogManager.getLogger(Journal.class);

    private final LinkedList<AbstractJournalEntry> entries = new LinkedList<>();

    /**
     * The caller has to take care that the entry timestamp is &gt;= the last timestamp!
     */
    void add(AbstractJournalEntry entry) {
        if (entry != null) {
            this.entries.add(entry);
        }
    }

    public LinkedList<AbstractJournalEntry> getEntries() {
        return this.getEntries(null, null);
    }

    /**
     * @param fromDate inclusive (can be null)
     * @param toDate exclusive (can be null)
     * @param events type of events or null/empty for any type
     * @return Journal entries, sorted ascending by date
     */
    public LinkedList<AbstractJournalEntry> getEntries(Date fromDate, Date toDate, Event... events) {
        final Set<Event> eventsAsSet = events == null || events.length == 0 ? Collections.emptySet() : new HashSet<>(Arrays.asList(events));

        if (fromDate == null && toDate == null && eventsAsSet.isEmpty()) {
            return this.entries;
        } else {
            LinkedList<AbstractJournalEntry> result = new LinkedList<>();

            for (AbstractJournalEntry e : this.entries) {
                if (fromDate == null || e.getTimestamp().compareTo(fromDate) >= 0) {
                    if (toDate == null || e.getTimestamp().compareTo(toDate) < 0) {
                        if (eventsAsSet.isEmpty() || eventsAsSet.contains(e.getEvent())) {
                            result.add(e);
                        }
                    }
                }
            }

            return result;
        }
    }

    public boolean isSystemVisited(String systemName) {
        return false; // TODO
    }

    public boolean isBodyScanned(String bodyName) {
        return false; // TODO
    }

}
