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
import java.util.List;
import java.util.stream.Stream;

/**
 * Reads the entire journal upon creation. Then watches the journal dir and
 * updates the journal as the files get updated.
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
        this.journal = this.init();
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

    private Journal init() throws IOException {
        logger.info("Reading old journal files...");
        //@formatter:off
        Stream<Path> journals = Files.list(this.journalDir)
                .filter(p -> p.getFileName().startsWith("Journal.") && p.getFileName().endsWith(".log"))
                .sorted((p1, p2) -> new Long(p1.toFile().lastModified()).compareTo(new Long(p2.toFile().lastModified())));
        //@formatter:on

        List<AbstractJournalEntry> entries = new ArrayList<>();
        journals.forEach(p -> {
            try {
                this.currentFilename = p.getFileName().toString();
                List<String> lines = Files.readAllLines(p, StandardCharsets.UTF_8);
                for (int lineNumber = 1; lineNumber <= lines.size(); lineNumber++) {
                    this.lastProcessedLineNumber = lineNumber;
                    try {
                        AbstractJournalEntry entry = this.reader.readJournalLine(lines.get(lineNumber - 1));
                        if (entry != null) {
                            entries.add(entry);
                        }
                    } catch (UnknownEventException e) {
                        // Ignore here (should have printed a warning)
                    }
                }
            } catch (Exception e) {
                throw new RuntimeException("Failed to init journal", e);
            }
        });

        logger.info("Constructing journal...");
        return new Journal(entries);
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
                            AbstractJournalEntry entry = this.reader.readJournalLine(lines.get(lineNumber - 1));
                            if (entry != null) {
                                for (JournalUpdateListener listener : this.listeners) {
                                    try {
                                        listener.onNewJournalEntry(entry);
                                    } catch (Exception e) {
                                        logger.warn(listener + " failed: " + e);
                                    }
                                }
                            }
                        } catch (UnknownEventException e) {
                            // Ignore here (should have printed a warning)
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
