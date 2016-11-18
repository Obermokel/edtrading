package borg.edtrading.gui;

import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Font;
import java.util.LinkedList;

import javax.swing.JList;

/**
 * JournalLogPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class JournalLogPanel extends JList<String> implements JournalUpdateListener {

    private static final long serialVersionUID = -5861060218896140151L;

    static final Logger logger = LogManager.getLogger(JournalLogPanel.class);

    private static final int HISTORY_SIZE = 100;
    private static final int VISIBLE_LINES = 10;

    private final LinkedList<String> history = new LinkedList<>();

    public JournalLogPanel(JournalReaderThread journalReaderThread) {
        this.setFont(new Font("Consolas", Font.PLAIN, 10));
        this.setVisibleRowCount(VISIBLE_LINES);

        journalReaderThread.addListener(this);
    }

    @Override
    public void onNewJournalLine(String line) {
        this.history.addFirst(line);
        while (this.history.size() > HISTORY_SIZE) {
            this.history.removeLast();
        }
        this.setListData(this.history.toArray(new String[this.history.size()]));
    }

    @Override
    public void onNewJournalEntry(AbstractJournalEntry entry) {
        // Do nothing
    }

}
