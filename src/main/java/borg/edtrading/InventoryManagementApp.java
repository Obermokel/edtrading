package borg.edtrading;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.journal.MaterialCollectedEntry;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;

/**
 * InventoryManagementApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryManagementApp {

    static final Logger logger = LogManager.getLogger(InventoryManagementApp.class);

    public static void main(String[] args) throws Exception {
        Journal journal = new Journal(JournalReader.readEntireJournal(Constants.JOURNAL_DIR));
        List<MaterialCollectedEntry> collectedEntries = MiscUtil.unsafeCast(journal.getEntries(null, null, Event.MaterialCollected));
        for (MaterialCollectedEntry e : collectedEntries) {
            //logger.debug(String.format(Locale.US, "%dx %s (%s)", e.getCount(), e.getName(), e.getCategory()));
        }
    }

}
