package borg.edtrading.journal;

import borg.edtrading.journal.entries.AbstractJournalEntry;

/**
 * JournalUpdateListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface JournalUpdateListener {

    void onNewJournalLine(String line);

    void onNewJournalEntry(AbstractJournalEntry entry);

}
