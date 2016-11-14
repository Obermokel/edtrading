package borg.edtrading.journal;

/**
 * JournalUpdateListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface JournalUpdateListener {

    void onNewJournalEntry(AbstractJournalEntry entry);

}
