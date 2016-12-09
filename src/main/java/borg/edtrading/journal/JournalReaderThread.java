package borg.edtrading.journal;

import borg.edtrading.journal.entries.AbstractJournalEntry;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.Date;
import java.util.List;

/**
 * JournalReaderThread
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class JournalReaderThread extends Thread {

    static final Logger logger = LogManager.getLogger(JournalReaderThread.class);

    private final Path journalDir;
    private final JournalReader reader;
    private final Journal journal;

    private String currentFilename = null;
    private int lastProcessedLineNumber = 0;
    private Date lastProcessedTimestamp = new Date(0);

    public JournalReaderThread(Path journalDir) throws IOException {
        this.setName("JournalReaderThread");
        this.setDaemon(false);

        this.journalDir = journalDir;
        this.reader = new JournalReader();
        this.journal = new Journal();
    }

    public void init() throws IOException {
        logger.info(this.getName() + " initializing...");
        //@formatter:off
        Files.list(this.journalDir)
                .filter(p -> p.getFileName().toString().startsWith("Journal.") && p.getFileName().toString().endsWith(".log"))
                .sorted((p1, p2) -> new Long(p1.toFile().lastModified()).compareTo(new Long(p2.toFile().lastModified())))
                .forEach(p -> this.updateJournal(p.getFileName().toString()));
        //@formatter:on
    }

    @Override
    public void run() {
        logger.info(this.getName() + " started");

        try (WatchService watcher = this.journalDir.getFileSystem().newWatchService()) {
            this.journalDir.register(watcher, StandardWatchEventKinds.ENTRY_MODIFY);

            while (!Thread.currentThread().isInterrupted()) {
                try {
                    final WatchKey key = watcher.take(); // Wait for the next event

                    for (WatchEvent<?> event : key.pollEvents()) {
                        final Kind<?> kind = event.kind();

                        if (kind == StandardWatchEventKinds.OVERFLOW) {
                            logger.warn("WatchService overflow for " + this.journalDir); // Print warning and continue
                        } else if (kind == StandardWatchEventKinds.ENTRY_MODIFY) {
                            this.updateJournal(event.context().toString());
                        }
                    }

                    if (!key.reset()) {
                        break; // Quit
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        } catch (Exception e) {
            throw new RuntimeException(this.getName() + " failed", e);
        }

        logger.info(this.getName() + " stopped");
    }

    private void updateJournal(String filename) {
        if (StringUtils.isNotEmpty(filename) && filename.startsWith("Journal.") && filename.endsWith(".log")) {
            try {
                int lineNumberBackup = this.lastProcessedLineNumber;
                String filenameBackup = this.currentFilename;

                if (!filename.equals(this.currentFilename)) {
                    this.lastProcessedLineNumber = 0; // New file
                }
                this.currentFilename = filename;

                boolean eventAdded = false;
                List<String> lines = Files.readAllLines(this.journalDir.resolve(filename), StandardCharsets.UTF_8);
                for (int lineNumber = 1; lineNumber <= lines.size(); lineNumber++) {
                    if (lineNumber > this.lastProcessedLineNumber) {
                        this.lastProcessedLineNumber = lineNumber;

                        try {
                            String line = lines.get(lineNumber - 1);
                            AbstractJournalEntry entry = this.reader.readJournalLine(line);

                            if (entry != null && entry.getTimestamp().compareTo(this.lastProcessedTimestamp) >= 0) {
                                this.lastProcessedTimestamp = entry.getTimestamp();

                                this.journal.add(entry);
                                eventAdded = true;
                            }
                        } catch (UnknownEventException e) {
                            logger.debug("Unknown event type '" + e.getEvent() + "' in line " + lineNumber + " of " + filename);
                        }
                    }
                }

                if (!eventAdded) {
                    this.currentFilename = filenameBackup;
                    this.lastProcessedLineNumber = lineNumberBackup;
                }
            } catch (IOException | RuntimeException e) {
                logger.error("Failed to read journal file " + this.currentFilename, e);
            }
        }
    }

    public Journal getJournal() {
        return this.journal;
    }

}
