package borg.edtrading.gui;

import borg.edtrading.journal.AbstractJournalEntry;
import borg.edtrading.journal.JournalUpdateListener;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Dimension;
import java.util.LinkedList;
import java.util.stream.Collectors;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

/**
 * RawJournalLogPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RawJournalLogPanel extends JPanel implements JournalUpdateListener {

    private static final long serialVersionUID = -937637107369684432L;

    static final Logger logger = LogManager.getLogger(RawJournalLogPanel.class);

    private static final int HISTORY_SIZE = 100;

    private final LinkedList<String> lastLines = new LinkedList<>();
    private final JTextArea log = new JTextArea();

    public RawJournalLogPanel() {
        this.log.setPreferredSize(new Dimension(1800, 200));
        this.add(new JScrollPane(this.log));
    }

    @Override
    public void onNewJournalLine(String line) {
        if (StringUtils.isNotEmpty(line)) {
            this.lastLines.addFirst(line);
            if (this.lastLines.size() > HISTORY_SIZE) {
                this.lastLines.removeLast();
            }
            //            this.log.setRows(this.lastLines.size());
            //            this.log.setColumns(this.lastLines.stream().map(s -> s.length()).max(null).get());
            this.log.setText(this.lastLines.stream().collect(Collectors.joining("\n")));
            //this.repaint();
        }
    }

    @Override
    public void onNewJournalEntry(AbstractJournalEntry entry) {
        // Do nothing
    }

}
