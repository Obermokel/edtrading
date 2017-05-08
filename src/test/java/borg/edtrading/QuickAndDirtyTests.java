package borg.edtrading;

import borg.edtrading.cfg.Config;
import borg.edtrading.cfg.Constants;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.util.List;

/**
 * QuickAndDirtyTests
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class QuickAndDirtyTests {

    static final Logger logger = LogManager.getLogger(QuickAndDirtyTests.class);

    public static void main(String[] args) throws Exception {
        AnnotationConfigApplicationContext appctx = new AnnotationConfigApplicationContext(Config.class);
        try {
            JournalReader journalReader = new JournalReader();
            List<AbstractJournalEntry> journal = journalReader.readEntireJournal(Constants.JOURNAL_DIR);
        } finally {
            appctx.close();
        }
    }

}
