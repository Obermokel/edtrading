package borg.edtrading.eddb.reader;

import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbCommodity;
import borg.edtrading.eddb.data.EddbEntity;
import borg.edtrading.eddb.data.EddbFaction;
import borg.edtrading.eddb.data.EddbMarketEntry;
import borg.edtrading.eddb.data.EddbModule;
import borg.edtrading.eddb.data.EddbStation;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.data.EdsmSystem;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.eddb.repositories.EddbCommodityRepository;
import borg.edtrading.eddb.repositories.EddbFactionRepository;
import borg.edtrading.eddb.repositories.EddbMarketEntryRepository;
import borg.edtrading.eddb.repositories.EddbModuleRepository;
import borg.edtrading.eddb.repositories.EddbStationRepository;
import borg.edtrading.eddb.repositories.EddbSystemRepository;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSyntaxException;
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.commons.lang3.mutable.MutableLong;
import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.domain.Sort.NullHandling;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.zip.GZIPInputStream;

import javax.annotation.PostConstruct;

/**
 * EddbReader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbReader {

    static final Logger logger = LogManager.getLogger(EddbReader.class);

    private static final File BASE_DIR = new File(System.getProperty("user.home"), ".eddbdata");

    @Autowired
    private ElasticsearchOperations esTemplate = null;
    @Autowired
    private EddbSystemRepository systemRepo = null;
    @Autowired
    private EddbBodyRepository bodyRepo = null;
    @Autowired
    private EddbStationRepository stationRepo = null;
    @Autowired
    private EddbFactionRepository factionRepo = null;
    @Autowired
    private EddbMarketEntryRepository marketEntryRepo = null;
    @Autowired
    private EddbCommodityRepository commodityRepo = null;
    @Autowired
    private EddbModuleRepository moduleRepo = null;

    @PostConstruct
    public void updateMappings() {
        if (this.esTemplate.indexExists(EddbSystem.class)) {
            try {
                this.esTemplate.putMapping(EddbSystem.class);
            } catch (Exception e) {
                logger.debug("Deleting system index");
                this.esTemplate.deleteIndex(EddbSystem.class);
                logger.debug("Creating system index");
                this.esTemplate.createIndex(EddbSystem.class);
            }
        }

        if (this.esTemplate.indexExists(EddbBody.class)) {
            try {
                this.esTemplate.putMapping(EddbBody.class);
            } catch (Exception e) {
                logger.debug("Deleting body index");
                this.esTemplate.deleteIndex(EddbBody.class);
                logger.debug("Creating body index");
                this.esTemplate.createIndex(EddbBody.class);
            }
        }

        if (this.esTemplate.indexExists(EddbStation.class)) {
            try {
                this.esTemplate.putMapping(EddbStation.class);
            } catch (Exception e) {
                logger.debug("Deleting station index");
                this.esTemplate.deleteIndex(EddbStation.class);
                logger.debug("Creating station index");
                this.esTemplate.createIndex(EddbStation.class);
            }
        }

        if (this.esTemplate.indexExists(EddbFaction.class)) {
            try {
                this.esTemplate.putMapping(EddbFaction.class);
            } catch (Exception e) {
                logger.debug("Deleting faction index");
                this.esTemplate.deleteIndex(EddbFaction.class);
                logger.debug("Creating faction index");
                this.esTemplate.createIndex(EddbFaction.class);
            }
        }

        if (this.esTemplate.indexExists(EddbMarketEntry.class)) {
            try {
                this.esTemplate.putMapping(EddbMarketEntry.class);
            } catch (Exception e) {
                logger.debug("Deleting market entry index");
                this.esTemplate.deleteIndex(EddbMarketEntry.class);
                logger.debug("Creating market entry index");
                this.esTemplate.createIndex(EddbMarketEntry.class);
            }
        }

        if (this.esTemplate.indexExists(EddbCommodity.class)) {
            try {
                this.esTemplate.putMapping(EddbCommodity.class);
            } catch (Exception e) {
                logger.debug("Deleting commodity index");
                this.esTemplate.deleteIndex(EddbCommodity.class);
                logger.debug("Creating commodity index");
                this.esTemplate.createIndex(EddbCommodity.class);
            }
        }

        if (this.esTemplate.indexExists(EddbModule.class)) {
            try {
                this.esTemplate.putMapping(EddbModule.class);
            } catch (Exception e) {
                logger.debug("Deleting module index");
                this.esTemplate.deleteIndex(EddbModule.class);
                logger.debug("Creating module index");
                this.esTemplate.createIndex(EddbModule.class);
            }
        }
    }

    public void loadEddbDataIntoElasticsearch(boolean forceReindex) throws IOException {
        if (!BASE_DIR.exists()) {
            BASE_DIR.mkdirs();
        }

        long start = System.currentTimeMillis();
        File commoditiesFile = new File(BASE_DIR, "commodities.json");
        boolean commoditiesUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/commodities.json", commoditiesFile);
        if (commoditiesUpdated || forceReindex || this.commodityRepo.count() <= 0) {
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(null, commoditiesFile, EddbCommodity.class, this.commodityRepo);
            this.deleteOldEntities("eddbcommodity", EddbCommodity.class, currentEntityIds);
        }
        File modulesFile = new File(BASE_DIR, "modules.json");
        boolean modulesUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/modules.json", modulesFile);
        if (modulesUpdated || forceReindex || this.moduleRepo.count() <= 0) {
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(null, modulesFile, EddbModule.class, this.moduleRepo);
            this.deleteOldEntities("eddbmodule", EddbModule.class, currentEntityIds);
        }
        File marketEntriesFile = new File(BASE_DIR, "listings.csv");
        boolean marketEntriesUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/listings.csv", marketEntriesFile);
        if (marketEntriesUpdated || forceReindex || this.marketEntryRepo.count() <= 0) {
            Page<EddbMarketEntry> first = this.marketEntryRepo.findAll(new PageRequest(0, 1, new Sort(new Sort.Order(Direction.DESC, "updatedAt", NullHandling.NULLS_LAST))));
            Date lastUpdate = first == null || first.getTotalElements() < 1 ? null : first.getContent().get(0).getUpdatedAt();
            Set<Long> currentEntityIds = this.readCsvFileIntoRepo(lastUpdate, marketEntriesFile, new EddbMarketEntryCsvRecordParser(), this.marketEntryRepo);
            this.deleteOldEntities("eddbmarketentry", EddbMarketEntry.class, currentEntityIds);
        }
        File systemsFile = new File(BASE_DIR, "systems.csv");
        File systemsPopulatedFile = new File(BASE_DIR, "systems_populated.jsonl");
        boolean systemsUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/systems.csv", systemsFile);
        boolean systemsPopulatedUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/systems_populated.jsonl", systemsPopulatedFile);
        if (systemsUpdated || systemsPopulatedUpdated || forceReindex || this.systemRepo.count() <= 0) {
            Page<EddbSystem> first = this.systemRepo.findAll(new PageRequest(0, 1, new Sort(new Sort.Order(Direction.DESC, "updatedAt", NullHandling.NULLS_LAST))));
            Date lastUpdate = first == null || first.getTotalElements() < 1 ? null : first.getContent().get(0).getUpdatedAt();
            File edsmFile = new File(BASE_DIR, "systemsWithCoordinates.json");
            this.downloadIfUpdated("https://www.edsm.net/dump/systemsWithCoordinates.json", edsmFile);
            Map<Long, EdsmSystem> edsmSystemsById = this.loadEdsmSystemsById(edsmFile);
            Set<Long> currentEntityIds = this.readCsvFileIntoRepo(lastUpdate, systemsFile, new EddbSystemCsvRecordParser(edsmSystemsById), this.systemRepo);
            currentEntityIds.addAll(this.readJsonFileIntoRepo(lastUpdate, systemsPopulatedFile, EddbSystem.class, this.systemRepo));
            this.deleteOldEntities("eddbsystem", EddbSystem.class, currentEntityIds);
        }
        File bodiesFile = new File(BASE_DIR, "bodies.jsonl");
        boolean bodiesUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/bodies.jsonl", bodiesFile);
        if (bodiesUpdated || forceReindex || this.bodyRepo.count() <= 0) {
            Page<EddbBody> first = this.bodyRepo.findAll(new PageRequest(0, 1, new Sort(new Sort.Order(Direction.DESC, "updatedAt", NullHandling.NULLS_LAST))));
            Date lastUpdate = first == null || first.getTotalElements() < 1 ? null : first.getContent().get(0).getUpdatedAt();
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(lastUpdate, bodiesFile, EddbBody.class, this.bodyRepo);
            this.deleteOldEntities("eddbbody", EddbBody.class, currentEntityIds);
        }
        File stationsFile = new File(BASE_DIR, "stations.jsonl");
        boolean stationsUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/stations.jsonl", stationsFile);
        if (stationsUpdated || forceReindex || this.stationRepo.count() <= 0) {
            Page<EddbStation> first = this.stationRepo.findAll(new PageRequest(0, 1, new Sort(new Sort.Order(Direction.DESC, "updatedAt", NullHandling.NULLS_LAST))));
            Date lastUpdate = first == null || first.getTotalElements() < 1 ? null : first.getContent().get(0).getUpdatedAt();
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(lastUpdate, stationsFile, EddbStation.class, this.stationRepo);
            this.deleteOldEntities("eddbstation", EddbStation.class, currentEntityIds);
        }
        File factionsFile = new File(BASE_DIR, "factions.jsonl");
        boolean factionsUpdated = this.downloadIfUpdated("https://eddb.io/archive/v5/factions.jsonl", factionsFile);
        if (factionsUpdated || forceReindex || this.factionRepo.count() <= 0) {
            Page<EddbFaction> first = this.factionRepo.findAll(new PageRequest(0, 1, new Sort(new Sort.Order(Direction.DESC, "updatedAt", NullHandling.NULLS_LAST))));
            Date lastUpdate = first == null || first.getTotalElements() < 1 ? null : first.getContent().get(0).getUpdatedAt();
            Set<Long> currentEntityIds = this.readJsonFileIntoRepo(lastUpdate, factionsFile, EddbFaction.class, this.factionRepo);
            this.deleteOldEntities("eddbfaction", EddbFaction.class, currentEntityIds);
        }
        this.enrichEddbData();
        long end = System.currentTimeMillis();
        logger.info("Downloaded data in " + DurationFormatUtils.formatDuration(end - start, "H:mm:ss"));
    }

    private Map<Long, EdsmSystem> loadEdsmSystemsById(File edsmFile) throws IOException {
        logger.debug("Reading " + edsmFile.getName());

        //@formatter:off
        final Gson gson = new GsonBuilder()
                //.registerTypeAdapter(Date.class, new SecondsSinceEpochDeserializer())
                //.registerTypeAdapter(Boolean.class, new BooleanDigitDeserializer())
                .setDateFormat("yyyy-MM-dd HH:mm:ss")
                .serializeNulls()
                .setPrettyPrinting()
                .create();
        //@formatter:on

        Map<Long, EdsmSystem> edsmSystemsById = new HashMap<>();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(edsmFile), "UTF-8"))) {
            EdsmSystem[] entities = (EdsmSystem[]) gson.fromJson(reader, Array.newInstance(EdsmSystem.class, 0).getClass());
            for (EdsmSystem entity : entities) {
                edsmSystemsById.put(entity.getId(), entity);
            }
        }

        return edsmSystemsById;
    }

    private <T extends EddbEntity> void deleteOldEntities(String index, Class<T> type, Set<Long> currentEntityIds) {
        logger.info("Deleting old entities from " + index);

        final DateFormat dfEta = new SimpleDateFormat("MMM dd @ HH:mm", Locale.US);
        long startBatch = System.currentTimeMillis();
        int n = 0;

        Set<Long> oldEntityIds = new HashSet<>();
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(QueryBuilders.matchAllQuery()).withIndices(index).withTypes(index).withPageable(new PageRequest(0, 1000)).build();
        String scrollId = esTemplate.scan(searchQuery, 300000, false);
        boolean hasRecords = true;
        while (hasRecords) {
            Page<T> page = esTemplate.scroll(scrollId, 300000, type);
            if (page.hasContent()) {
                for (T entity : page.getContent()) {
                    if (!currentEntityIds.contains(entity.getId())) {
                        oldEntityIds.add(entity.getId());
                    }

                    if (++n % 100000 == 0) {
                        long millis = System.currentTimeMillis() - startBatch;
                        double entitiesPerSec = (100000d / Math.max(1, millis)) * 1000d;
                        int entitiesRemaining = (int) page.getTotalElements() - n;
                        double secondsRemaining = entitiesRemaining / entitiesPerSec;
                        Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                        logger.info(String.format(Locale.US, "Checked %,d of %,d %s (%.1f/sec) -- ETA %s", n, page.getTotalElements(), index, entitiesPerSec, dfEta.format(eta)));
                        startBatch = System.currentTimeMillis();
                    }
                }
            } else {
                hasRecords = false;
            }
        }
        esTemplate.clearScroll(scrollId);

        for (Long oldEntityId : oldEntityIds) {
            esTemplate.delete(type, String.valueOf(oldEntityId));
        }

        logger.info("Deleted " + oldEntityIds.size() + " entities from " + index);
    }

    private void enrichEddbData() {
        final DateFormat dfEta = new SimpleDateFormat("MMM dd @ HH:mm", Locale.US);

        BoolQueryBuilder qb = QueryBuilders.boolQuery();
        qb.must(QueryBuilders.matchAllQuery());
        qb.mustNot(QueryBuilders.existsQuery("coord.x"));

        logger.debug("Setting body coords...");
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withIndices("eddbbody").withQuery(qb).withPageable(new PageRequest(0, 1000)).build();
        String scrollId = esTemplate.scan(searchQuery, 300000, false);
        boolean hasRecords = true;
        MutableLong startBatch = new MutableLong(System.currentTimeMillis());
        MutableInt n = new MutableInt(0);
        while (hasRecords) {
            Page<EddbBody> page = esTemplate.scroll(scrollId, 300000, EddbBody.class);
            if (page.hasContent()) {
                List<EddbBody> content = page.getContent();
                content.parallelStream().forEach(c -> {
                    c.setStarClass(c.toStarClass());
                    EddbSystem system = systemRepo.findOne(c.getSystemId());
                    if (system != null) {
                        c.setCoord(system.getCoord());
                    }
                    n.increment();
                    if (n.intValue() % 100000 == 0) {
                        long millis = System.currentTimeMillis() - startBatch.longValue();
                        double entitiesPerSec = (100000d / Math.max(1, millis)) * 1000d;
                        int entitiesRemaining = (int) page.getTotalElements() - n.intValue();
                        double secondsRemaining = entitiesRemaining / entitiesPerSec;
                        Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                        logger.info(String.format(Locale.US, "Processed %,d of %,d %s (%.1f/sec) -- ETA %s", n.intValue(), page.getTotalElements(), "bodies", entitiesPerSec, dfEta.format(eta)));
                        startBatch.setValue(System.currentTimeMillis());
                    }
                });
                bodyRepo.save(content);
            } else {
                hasRecords = false;
            }
        }
        esTemplate.clearScroll(scrollId);

        logger.debug("Setting station coords...");
        searchQuery = new NativeSearchQueryBuilder().withIndices("eddbstation").withQuery(qb).withPageable(new PageRequest(0, 1000)).build();
        scrollId = esTemplate.scan(searchQuery, 300000, false);
        hasRecords = true;
        startBatch.setValue(System.currentTimeMillis());
        n.setValue(0);
        while (hasRecords) {
            Page<EddbStation> page = esTemplate.scroll(scrollId, 300000, EddbStation.class);
            if (page.hasContent()) {
                List<EddbStation> content = page.getContent();
                content.parallelStream().forEach(c -> {
                    EddbSystem system = systemRepo.findOne(c.getSystemId());
                    if (system != null) {
                        c.setCoord(system.getCoord());
                    }
                    n.increment();
                    if (n.intValue() % 100000 == 0) {
                        long millis = System.currentTimeMillis() - startBatch.longValue();
                        double entitiesPerSec = (100000d / Math.max(1, millis)) * 1000d;
                        int entitiesRemaining = (int) page.getTotalElements() - n.intValue();
                        double secondsRemaining = entitiesRemaining / entitiesPerSec;
                        Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                        logger.info(String.format(Locale.US, "Processed %,d of %,d %s (%.1f/sec) -- ETA %s", n.intValue(), page.getTotalElements(), "stations", entitiesPerSec, dfEta.format(eta)));
                        startBatch.setValue(System.currentTimeMillis());
                    }
                });
                stationRepo.save(content);
            } else {
                hasRecords = false;
            }
        }
        esTemplate.clearScroll(scrollId);

        logger.debug("Setting faction home coords...");
        searchQuery = new NativeSearchQueryBuilder().withIndices("eddbfaction").withQuery(qb).withPageable(new PageRequest(0, 1000)).build();
        scrollId = esTemplate.scan(searchQuery, 300000, false);
        hasRecords = true;
        startBatch.setValue(System.currentTimeMillis());
        n.setValue(0);
        while (hasRecords) {
            Page<EddbFaction> page = esTemplate.scroll(scrollId, 300000, EddbFaction.class);
            if (page.hasContent()) {
                List<EddbFaction> content = page.getContent();
                content.parallelStream().forEach(c -> {
                    EddbSystem system = systemRepo.findOne(c.getHomeSystemId());
                    if (system != null) {
                        c.setCoord(system.getCoord());
                    }
                    n.increment();
                    if (n.intValue() % 100000 == 0) {
                        long millis = System.currentTimeMillis() - startBatch.longValue();
                        double entitiesPerSec = (100000d / Math.max(1, millis)) * 1000d;
                        int entitiesRemaining = (int) page.getTotalElements() - n.intValue();
                        double secondsRemaining = entitiesRemaining / entitiesPerSec;
                        Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                        logger.info(String.format(Locale.US, "Processed %,d of %,d %s (%.1f/sec) -- ETA %s", n.intValue(), page.getTotalElements(), "factions", entitiesPerSec, dfEta.format(eta)));
                        startBatch.setValue(System.currentTimeMillis());
                    }
                });
                factionRepo.save(content);
            } else {
                hasRecords = false;
            }
        }
        esTemplate.clearScroll(scrollId);

        logger.debug("Setting market entry coords...");
        searchQuery = new NativeSearchQueryBuilder().withIndices("eddbmarketentry").withQuery(qb).withPageable(new PageRequest(0, 1000)).build();
        scrollId = esTemplate.scan(searchQuery, 300000, false);
        hasRecords = true;
        startBatch.setValue(System.currentTimeMillis());
        n.setValue(0);
        while (hasRecords) {
            Page<EddbMarketEntry> page = esTemplate.scroll(scrollId, 300000, EddbMarketEntry.class);
            if (page.hasContent()) {
                List<EddbMarketEntry> content = page.getContent();
                content.parallelStream().forEach(c -> {
                    EddbStation station = stationRepo.findOne(c.getStationId());
                    if (station != null) {
                        c.setCoord(station.getCoord());
                    }
                    n.increment();
                    if (n.intValue() % 100000 == 0) {
                        long millis = System.currentTimeMillis() - startBatch.longValue();
                        double entitiesPerSec = (100000d / Math.max(1, millis)) * 1000d;
                        int entitiesRemaining = (int) page.getTotalElements() - n.intValue();
                        double secondsRemaining = entitiesRemaining / entitiesPerSec;
                        Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                        logger.info(String.format(Locale.US, "Processed %,d of %,d %s (%.1f/sec) -- ETA %s", n.intValue(), page.getTotalElements(), "market entries", entitiesPerSec, dfEta.format(eta)));
                        startBatch.setValue(System.currentTimeMillis());
                    }
                });
                marketEntryRepo.save(content);
            } else {
                hasRecords = false;
            }
        }
        esTemplate.clearScroll(scrollId);
    }

    private <T extends EddbEntity> Set<Long> readCsvFileIntoRepo(Date lastUpdate, File file, CSVRecordParser<T> csvRecordParser, ElasticsearchRepository<T, Long> repo) throws IOException {
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
                T parsed = csvRecordParser.parse(record);
                if (lastUpdate == null || parsed.getUpdatedAt() == null || parsed.getUpdatedAt().after(lastUpdate)) {
                    batch.add(parsed);
                } else {
                    savedEntityIds.add(parsed.getId());
                }
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
            if (!batch.isEmpty()) {
                for (T entity : repo.save(batch)) {
                    savedEntityIds.add(entity.getId());
                }
            }
            batch.clear();
        }

        return savedEntityIds;
    }

    private <T extends EddbEntity> Set<Long> readJsonFileIntoRepo(Date lastUpdate, File file, Class<T> type, ElasticsearchRepository<T, Long> repo) throws IOException {
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
                        T parsed = gson.fromJson(line, type);
                        if (lastUpdate == null || parsed.getUpdatedAt() == null || parsed.getUpdatedAt().after(lastUpdate)) {
                            batch.add(parsed);
                        } else {
                            savedEntityIds.add(parsed.getId());
                        }
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
                if (!batch.isEmpty()) {
                    for (T entity : repo.save(batch)) {
                        savedEntityIds.add(entity.getId());
                    }
                }
                batch.clear();
            } else {
                T[] entities = (T[]) gson.fromJson(reader, Array.newInstance(type, 0).getClass());

                long startBatch = System.currentTimeMillis();
                int n = 0;
                List<T> batch = new ArrayList<>(batchSize);
                for (T loadedEntity : entities) {
                    if (lastUpdate == null || loadedEntity.getUpdatedAt() == null || loadedEntity.getUpdatedAt().after(lastUpdate)) {
                        batch.add(loadedEntity);
                    } else {
                        savedEntityIds.add(loadedEntity.getId());
                    }
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
                if (!batch.isEmpty()) {
                    for (T entity : repo.save(batch)) {
                        savedEntityIds.add(entity.getId());
                    }
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
