package borg.edtrading.journal;

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
import java.util.ArrayList;
import java.util.Collections;
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
    private final List<JournalUpdateListener> listeners = new ArrayList<>();

    public JournalReaderThread(Path journalDir) throws IOException {
        this.setName("JournalReaderThread");
        this.setDaemon(true);

        this.journalDir = journalDir;
        this.reader = new JournalReader();
        this.journal = new Journal(Collections.emptyList());
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
                if (!filename.equals(this.currentFilename)) {
                    this.lastProcessedLineNumber = 0; // New file
                }
                this.currentFilename = filename;

                List<String> lines = Files.readAllLines(this.journalDir.resolve(filename), StandardCharsets.UTF_8);
                for (int lineNumber = 1; lineNumber <= lines.size(); lineNumber++) {
                    if (lineNumber > this.lastProcessedLineNumber) {
                        this.lastProcessedLineNumber = lineNumber;

                        try {
                            String line = lines.get(lineNumber - 1);
                            for (JournalUpdateListener listener : this.listeners) {
                                try {
                                    listener.onNewJournalLine(line);
                                } catch (Exception e) {
                                    logger.warn(listener + " failed: " + e);
                                }
                            }
                            AbstractJournalEntry entry = this.reader.readJournalLine(line);

                            if (entry != null) {
                                this.journal.add(entry);

                                for (JournalUpdateListener listener : this.listeners) {
                                    try {
                                        listener.onNewJournalEntry(entry);
                                    } catch (Exception e) {
                                        logger.warn(listener + " failed: " + e);
                                    }
                                }
                            }
                        } catch (UnknownEventException e) {
                            logger.trace("Unknown event type '" + e.getEvent() + "' in line " + lineNumber + " of " + filename);
                        }
                    }
                }
            } catch (IOException | RuntimeException e) {
                logger.error("Failed to read journal file " + this.currentFilename, e);
            }
        }
    }

    public Journal getJournal() {
        return this.journal;
    }

    public boolean addListener(JournalUpdateListener listener) {
        if (listener == null || this.listeners.contains(listener)) {
            return false;
        } else {
            return this.listeners.add(listener);
        }
    }

    public boolean removeListener(JournalUpdateListener listener) {
        if (listener == null) {
            return false;
        } else {
            return this.listeners.remove(listener);
        }
    }

}
