package borg.edtrading.journal;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Journal
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Journal {

    static final Logger logger = LogManager.getLogger(Journal.class);

    private final List<AbstractJournalEntry> entries;
    private final Set<String> visitedSystems;
    private final Set<String> scannedBodies;

    public Journal(List<AbstractJournalEntry> entries) {
        this.entries = new ArrayList<>(entries);

        Collections.sort(this.entries); // We want it sorted

        this.visitedSystems = entries.stream().filter(e -> e.getEvent() == Event.Location).map(e -> ((LocationEntry) e).getStarSystem()).collect(Collectors.toSet());
        this.scannedBodies = entries.stream().filter(e -> e.getEvent() == Event.Scan).map(e -> ((ScanEntry) e).getBodyName()).collect(Collectors.toSet());
    }

    public List<AbstractJournalEntry> getEntries() {
        return Collections.unmodifiableList(this.entries);
    }

    public Set<String> getVisitedSystems() {
        return Collections.unmodifiableSet(this.visitedSystems);
    }

    public Set<String> getScannedBodies() {
        return Collections.unmodifiableSet(this.scannedBodies);
    }

}
