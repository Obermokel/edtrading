package borg.edtrading;

import borg.edtrading.gui.InventoryPanel;
import borg.edtrading.journal.JournalReaderThread;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;

import javax.swing.JFrame;

/**
 * SidePanelApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SidePanelApp {

    static final Logger logger = LogManager.getLogger(SidePanelApp.class);

    public static void main(String[] args) throws IOException {
        String commander = "Mokel DeLorean";
        Path journalDir = Paths.get(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Journal"); // TODO Use live dir

        // Create the reader thread which will initially read the entire journal
        final JournalReaderThread journalReaderThread = new JournalReaderThread(journalDir.toFile());

        // Create all panels and register them to be notified by the reader thread
        InventoryPanel inventoryPanel = new InventoryPanel(journalReaderThread.getJournal(), commander);
        journalReaderThread.addListener(inventoryPanel);

        // Construct the window with all panels
        JFrame frame = new JFrame("SidePanel");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.add(inventoryPanel);
        frame.pack();
        frame.setVisible(true);

        // Start the reader thread
        journalReaderThread.start();

        // Watch the journal
        // TODO Move into reader thread
        try (WatchService watcher = journalDir.getFileSystem().newWatchService()) {
            journalDir.register(watcher, StandardWatchEventKinds.ENTRY_MODIFY);
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    final WatchKey key = watcher.take(); // Wait for the next event

                    for (WatchEvent<?> event : key.pollEvents()) {
                        final Kind<?> kind = event.kind();

                        if (kind == StandardWatchEventKinds.OVERFLOW) {
                            logger.warn("WatchService overflow for " + journalDir); // Print warning and continue
                        } else {
                            WatchEvent<Path> pathEvent = (WatchEvent<Path>) event;
                            Path file = journalDir.resolve(pathEvent.context());

                            if (kind == StandardWatchEventKinds.ENTRY_MODIFY) {
                                journalReaderThread.notifyJournalUpdated(file.toFile().getName());
                            }
                        }
                    }

                    if (!key.reset()) {
                        break; // Quit
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }
    }

}
