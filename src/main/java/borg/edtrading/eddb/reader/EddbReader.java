package borg.edtrading.eddb.reader;

import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbCommodity;
import borg.edtrading.eddb.data.EddbEntity;
import borg.edtrading.eddb.data.EddbFaction;
import borg.edtrading.eddb.data.EddbMarketEntry;
import borg.edtrading.eddb.data.EddbModule;
import borg.edtrading.eddb.data.EddbStation;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.eddb.repositories.EddbCommodityRepository;
import borg.edtrading.eddb.repositories.EddbFactionRepository;
import borg.edtrading.eddb.repositories.EddbMarketEntryRepository;
import borg.edtrading.eddb.repositories.EddbModuleRepository;
import borg.edtrading.eddb.repositories.EddbStationRepository;
import borg.edtrading.eddb.repositories.EddbSystemRepository;
import borg.edtrading.services.EddbService;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSyntaxException;
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Array;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.zip.GZIPInputStream;

/**
 * EddbReader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbReader {

    static final Logger logger = LogManager.getLogger(EddbReader.class);

    private static final File BASE_DIR = new File(System.getProperty("user.home"), ".eddbdata");

    @Autowired
    private ApplicationContext appctx = null;

    public void loadEddbDataIntoElasticsearch() throws IOException {
        if (!BASE_DIR.exists()) {
            BASE_DIR.mkdirs();
        }

        long start = System.currentTimeMillis();
        File commoditiesFile = new File(BASE_DIR, "commodities.json");
        if (this.downloadIfUpdated("https://eddb.io/archive/v5/commodities.json", commoditiesFile)) {
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(commoditiesFile, EddbCommodity.class, this.appctx.getBean(EddbCommodityRepository.class));
            this.appctx.getBean(EddbService.class).deleteOldEntities("eddbcommodity", EddbCommodity.class, currentEntityIds);
        }
        File modulesFile = new File(BASE_DIR, "modules.json");
        if (this.downloadIfUpdated("https://eddb.io/archive/v5/modules.json", modulesFile)) {
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(modulesFile, EddbModule.class, this.appctx.getBean(EddbModuleRepository.class));
            this.appctx.getBean(EddbService.class).deleteOldEntities("eddbmodule", EddbModule.class, currentEntityIds);
        }
        File marketEntriesFile = new File(BASE_DIR, "listings.csv");
        if (this.downloadIfUpdated("https://eddb.io/archive/v5/listings.csv", marketEntriesFile)) {
            Set<Long> currentEntityIds = this.readCsvFileIntoRepo(marketEntriesFile, new EddbMarketEntryCsvRecordParser(), this.appctx.getBean(EddbMarketEntryRepository.class));
            this.appctx.getBean(EddbService.class).deleteOldEntities("eddbmarketentry", EddbMarketEntry.class, currentEntityIds);
        }
        File systemsFile = new File(BASE_DIR, "systems.csv");
        File systemsPopulatedFile = new File(BASE_DIR, "systems_populated.jsonl");
        boolean systemsUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/systems.csv", systemsFile);
        boolean systemsPopulatedUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/systems_populated.jsonl", systemsPopulatedFile);
        if (systemsUpdated || systemsPopulatedUpdated) {
            Set<Long> currentEntityIds = this.readCsvFileIntoRepo(systemsFile, new EddbSystemCsvRecordParser(), this.appctx.getBean(EddbSystemRepository.class));
            currentEntityIds.addAll(this.readJsonFileIntoRepo(systemsPopulatedFile, EddbSystem.class, this.appctx.getBean(EddbSystemRepository.class)));
            this.appctx.getBean(EddbService.class).deleteOldEntities("eddbsystem", EddbSystem.class, currentEntityIds);
        }
        File bodiesFile = new File(BASE_DIR, "bodies.jsonl");
        if (this.downloadIfUpdated("https://eddb.io/archive/v5/bodies.jsonl", bodiesFile)) {
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(bodiesFile, EddbBody.class, this.appctx.getBean(EddbBodyRepository.class));
            this.appctx.getBean(EddbService.class).deleteOldEntities("eddbbody", EddbBody.class, currentEntityIds);
        }
        File stationsFile = new File(BASE_DIR, "stations.jsonl");
        if (this.downloadIfUpdated("https://eddb.io/archive/v5/stations.jsonl", stationsFile)) {
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(stationsFile, EddbStation.class, this.appctx.getBean(EddbStationRepository.class));
            this.appctx.getBean(EddbService.class).deleteOldEntities("eddbstation", EddbStation.class, currentEntityIds);
        }
        File factionsFile = new File(BASE_DIR, "factions.jsonl");
        if (this.downloadIfUpdated("https://eddb.io/archive/v5/factions.jsonl", factionsFile)) {
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(factionsFile, EddbFaction.class, this.appctx.getBean(EddbFactionRepository.class));
            this.appctx.getBean(EddbService.class).deleteOldEntities("eddbfaction", EddbFaction.class, currentEntityIds);
        }
        this.appctx.getBean(EddbService.class).setMissingCoords();
        long end = System.currentTimeMillis();
        logger.info("Downloaded data in " + DurationFormatUtils.formatDuration(end - start, "H:mm:ss"));
    }

    private <T extends EddbEntity> Set<Long> readCsvFileIntoRepo(File file, CSVRecordParser<T> csvRecordParser, ElasticsearchRepository<T, Long> repo) throws IOException {
        Set<Long> savedEntityIds = new HashSet<>();

        final DateFormat dfEta = new SimpleDateFormat("MMM dd @ HH:mm", Locale.US);
        final int batchSize = 10000;
        final int total = this.countLines(file) - 1;

        logger.debug("Reading " + file.getName());
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))) {
            Iterable<CSVRecord> records = CSVFormat.DEFAULT.withHeader().parse(reader);

            long startBatch = System.currentTimeMillis();
            int n = 0;
            List<T> batch = new ArrayList<>(batchSize);
            for (CSVRecord record : records) {
                batch.add(csvRecordParser.parse(record));
                if (batch.size() >= batchSize) {
                    for (T entity : repo.save(batch)) {
                        savedEntityIds.add(entity.getId());
                    }
                    batch.clear();
                }
                if (++n % 100000 == 0) {
                    long millis = System.currentTimeMillis() - startBatch;
                    double entitiesPerSec = (100000d / Math.max(1, millis)) * 1000d;
                    int entitiesRemaining = total - n;
                    double secondsRemaining = entitiesRemaining / entitiesPerSec;
                    Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                    logger.info(String.format(Locale.US, "Imported %,d of %,d %s (%.1f/sec) -- ETA %s", n, total, file.getName().substring(0, file.getName().lastIndexOf(".")), entitiesPerSec, dfEta.format(eta)));
                    startBatch = System.currentTimeMillis();
                }
            }
            for (T entity : repo.save(batch)) {
                savedEntityIds.add(entity.getId());
            }
            batch.clear();
        }

        return savedEntityIds;
    }

    private <T extends EddbEntity> Set<Long> readJsonFileIntoRepo(File file, Class<T> type, ElasticsearchRepository<T, Long> repo) throws IOException {
        Set<Long> savedEntityIds = new HashSet<>();

        final DateFormat dfEta = new SimpleDateFormat("MMM dd @ HH:mm", Locale.US);
        final int batchSize = 10000;
        final int total = this.countLines(file) - 1;

        //@formatter:off
        final Gson gson = new GsonBuilder()
                .registerTypeAdapter(Date.class, new SecondsSinceEpochDeserializer())
                .registerTypeAdapter(Boolean.class, new BooleanDigitDeserializer())
                .setDateFormat("yyyy-MM-dd HH:mm:ss")
                .serializeNulls()
                .setPrettyPrinting()
                .create();
        //@formatter:on

        logger.debug("Reading " + file.getName());
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))) {
            if (file.getName().endsWith(".jsonl")) {
                long startBatch = System.currentTimeMillis();
                int n = 0;
                List<T> batch = new ArrayList<>(batchSize);
                String line = reader.readLine();
                while (line != null) {
                    try {
                        batch.add(gson.fromJson(line, type));
                        if (batch.size() >= batchSize) {
                            for (T entity : repo.save(batch)) {
                                savedEntityIds.add(entity.getId());
                            }
                            batch.clear();
                        }
                        if (++n % 100000 == 0) {
                            long millis = System.currentTimeMillis() - startBatch;
                            double entitiesPerSec = (100000d / Math.max(1, millis)) * 1000d;
                            int entitiesRemaining = total - n;
                            double secondsRemaining = entitiesRemaining / entitiesPerSec;
                            Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                            logger.info(String.format(Locale.US, "Imported %,d of %,d %s (%.1f/sec) -- ETA %s", n, total, file.getName().substring(0, file.getName().lastIndexOf(".")), entitiesPerSec, dfEta.format(eta)));
                            startBatch = System.currentTimeMillis();
                        }
                    } catch (JsonSyntaxException e) {
                        logger.warn("Corrupt line in " + file + ": " + line, e);
                    }
                    line = reader.readLine();
                }
                for (T entity : repo.save(batch)) {
                    savedEntityIds.add(entity.getId());
                }
                batch.clear();
            } else {
                T[] entities = (T[]) gson.fromJson(reader, Array.newInstance(type, 0).getClass());

                long startBatch = System.currentTimeMillis();
                int n = 0;
                List<T> batch = new ArrayList<>(batchSize);
                for (T loadedEntity : entities) {
                    batch.add(loadedEntity);
                    if (batch.size() >= batchSize) {
                        for (T entity : repo.save(batch)) {
                            savedEntityIds.add(entity.getId());
                        }
                        batch.clear();
                    }
                    if (++n % 100000 == 0) {
                        long millis = System.currentTimeMillis() - startBatch;
                        double entitiesPerSec = (100000d / Math.max(1, millis)) * 1000d;
                        int entitiesRemaining = total - n;
                        double secondsRemaining = entitiesRemaining / entitiesPerSec;
                        Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                        logger.info(String.format(Locale.US, "Imported %,d of %,d %s (%.1f/sec) -- ETA %s", n, total, file.getName().substring(0, file.getName().lastIndexOf(".")), entitiesPerSec, dfEta.format(eta)));
                        startBatch = System.currentTimeMillis();
                    }
                }
                for (T entity : repo.save(batch)) {
                    savedEntityIds.add(entity.getId());
                }
                batch.clear();
            }
        }

        return savedEntityIds;
    }

    private int countLines(File file) throws IOException {
        int total = 0;
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))) {
            while (reader.readLine() != null) {
                total++;
            }
        }
        return total;
    }

    private boolean downloadIfUpdated(String url, File file) throws IOException {
        //if (!file.exists() || (System.currentTimeMillis() - file.lastModified() > DateUtils.MILLIS_PER_DAY / 2L && headLastModified(url) > file.lastModified())) {
        if (!file.exists() || headLastModified(url) > file.lastModified()) {
            backup(file);
            download(url, file);
            return true;
        } else {
            return false;
        }
    }

    private long backup(File sourceFile) throws IOException {
        File backupDir = new File(BASE_DIR, "backup");
        if (!backupDir.exists()) {
            backupDir.mkdirs();
        }

        InputStream in = null;
        ZipArchiveOutputStream out = null;
        try {
            String basename = sourceFile.getName().substring(0, sourceFile.getName().lastIndexOf(".")); // w/o dot
            String extension = sourceFile.getName().substring(sourceFile.getName().lastIndexOf(".")); // w/ dot
            String lastModified = new SimpleDateFormat("yyyy-MM-dd").format(sourceFile.lastModified());
            File backupFile = new File(backupDir, basename + "." + lastModified + ".zip");

            if (!sourceFile.exists() || backupFile.exists()) {
                return 0L;
            } else {
                in = new FileInputStream(sourceFile);
                out = new ZipArchiveOutputStream(backupFile);
                out.putArchiveEntry(out.createArchiveEntry(sourceFile, basename + "." + lastModified + extension));
                logger.debug(String.format(Locale.US, "Backup of %s to %s (%.1f MB)", sourceFile.getName(), backupFile.getName(), sourceFile.length() / 1048576.0));
                long bytes = IOUtils.copyLarge(in, out);
                out.closeArchiveEntry();
                return bytes;
            }
        } finally {
            if (out != null) {
                out.close();
            }
            if (in != null) {
                in.close();
            }
        }
    }

    private long download(String url, File file) throws IOException {
        HttpURLConnection conn = null;
        InputStream in = null;
        OutputStream out = null;
        try {
            conn = (HttpURLConnection) new URL(url).openConnection();
            conn.setRequestMethod("GET");
            conn.setRequestProperty("Accept-Encoding", "gzip");
            int responseCode = conn.getResponseCode();
            String responseMessage = conn.getResponseMessage();
            if (responseCode != 200) {
                throw new IOException("Response Code " + responseCode + " (" + responseMessage + ")");
            } else {
                if ("gzip".equals(conn.getContentEncoding())) {
                    in = new GZIPInputStream(conn.getInputStream());
                } else {
                    in = conn.getInputStream();
                }
                out = new FileOutputStream(file, false);
                logger.debug(String.format(Locale.US, "Download of %s (%.1f MB)", url, headContentLength(url) / 1048576.0));
                return IOUtils.copyLarge(in, out);
            }
        } finally {
            if (out != null) {
                out.close();
            }
            if (in != null) {
                in.close();
            }
            if (conn != null) {
                conn.disconnect();
            }
        }
    }

    private long headLastModified(String url) throws IOException {
        HttpURLConnection conn = null;
        try {
            conn = (HttpURLConnection) new URL(url).openConnection();
            conn.setRequestMethod("HEAD");
            return conn.getLastModified();
        } finally {
            if (conn != null) {
                conn.disconnect();
            }
        }
    }

    private long headContentLength(String url) throws IOException {
        HttpURLConnection conn = null;
        try {
            conn = (HttpURLConnection) new URL(url).openConnection();
            conn.setRequestMethod("HEAD");
            return conn.getContentLengthLong();
        } finally {
            if (conn != null) {
                conn.disconnect();
            }
        }
    }

}
