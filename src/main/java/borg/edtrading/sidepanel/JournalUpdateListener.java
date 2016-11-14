package borg.edtrading.sidepanel;

import borg.edtrading.journal.AbstractJournalEntry;

/**
 * JournalUpdateListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface JournalUpdateListener {

    void onNewJournalEntry(AbstractJournalEntry entry);

}
