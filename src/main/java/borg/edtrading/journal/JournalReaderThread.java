package borg.edtrading.journal;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/**
 * Reads the entire journal upon creation. Then waits until TODO
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class JournalReaderThread extends Thread {

    static final Logger logger = LogManager.getLogger(JournalReaderThread.class);

    private final File journalDir;
    private final JournalReader reader;
    private final Journal journal;

    private String currentFilename = null;
    private int lastProcessedLineNumber = 0;
    private final List<JournalUpdateListener> listeners = new ArrayList<>();

    public JournalReaderThread(File journalDir) throws IOException {
        this.setName("JournalUpdateHandler");
        this.setDaemon(true);

        this.journalDir = journalDir;
        this.reader = new JournalReader();
        this.journal = this.init();
    }

    @Override
    public void run() {
        logger.info(this.getName() + " started");

        while (!Thread.currentThread().isInterrupted()) {
            synchronized (this) {
                try {
                    this.wait();
                    this.updateJournal();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }

        logger.info(this.getName() + " stopped");
    }

    private Journal init() throws IOException {
        logger.info("Reading old journal files...");

        File[] journalFiles = this.journalDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().startsWith("Journal.") && file.getName().endsWith(".log");
            }
        });
        Arrays.sort(journalFiles, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return new Long(f1.lastModified()).compareTo(new Long(f2.lastModified()));
            }
        });

        List<AbstractJournalEntry> entries = new ArrayList<>();
        for (File journalFile : journalFiles) {
            this.currentFilename = journalFile.getName();
            List<String> lines = FileUtils.readLines(journalFile, "UTF-8");
            for (int lineNumber = 1; lineNumber <= lines.size(); lineNumber++) {
                this.lastProcessedLineNumber = lineNumber;
                try {
                    AbstractJournalEntry entry = this.reader.readJournalLine(lines.get(lineNumber - 1));
                    if (entry != null) {
                        entries.add(entry);
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

        logger.info("Constructing journal...");
        return new Journal(entries);
    }

    private void updateJournal() {
        try {
            List<String> lines = FileUtils.readLines(new File(this.journalDir, this.currentFilename), "UTF-8");
            for (int lineNumber = 1; lineNumber <= lines.size(); lineNumber++) {
                if (lineNumber > this.lastProcessedLineNumber) {
                    this.lastProcessedLineNumber = lineNumber;
                    try {
                        AbstractJournalEntry entry = this.reader.readJournalLine(lines.get(lineNumber - 1));
                        if (entry != null) {
                            // TODO
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

    public Journal getJournal() {
        return this.journal;
    }

    public void notifyJournalUpdated(String filename) {
        if (StringUtils.isNotEmpty(filename) && filename.startsWith("Journal.") && filename.endsWith(".log")) {
            synchronized (this) {
                if (!filename.equals(this.currentFilename)) {
                    this.lastProcessedLineNumber = 0; // New file
                }
                this.currentFilename = filename;
                this.notify();
            }
        }
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
