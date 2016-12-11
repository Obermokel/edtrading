package borg.edtrading.sidepanel;

import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * GameSession
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GameSession implements Serializable {

    private static final long serialVersionUID = -4069666534861037328L;

    static final Logger logger = LogManager.getLogger(GameSession.class);

    private final List<GameSessionListener> listeners = new ArrayList<>();

    public GameSession(JournalReaderThread journalReaderThread) {
    }

    public boolean addListener(GameSessionListener listener) {
        if (listener == null || this.listeners.contains(listener)) {
            return false;
        } else {
            return this.listeners.add(listener);
        }
    }

    public boolean removeListener(GameSessionListener listener) {
        if (listener == null) {
            return false;
        } else {
            return this.listeners.remove(listener);
        }
    }

    private void onNewJournalEntry(AbstractJournalEntry entry) {
        try {

        } catch (Exception e) {
            logger.error("Failed to handle " + entry, e);
        }
    }

}
