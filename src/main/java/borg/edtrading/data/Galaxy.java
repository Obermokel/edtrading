package borg.edtrading.data;

import borg.edtrading.Constants;
import borg.edtrading.json.BooleanDigitDeserializer;
import borg.edtrading.json.SecondsSinceEpochDeserializer;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Galaxy
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Galaxy {

    static final Logger logger = LogManager.getLogger(Galaxy.class);

    //    private final List<Rare> rares;
    //    private final Map<Long, Commodity> commoditiesById;
    private final Map<Long, StarSystem> starSystemsById;
    //    private final Map<Long, Station> stationsById;
    //    private final Map<Long, List<Station>> stationsByStarSystemId;
    //    private final Map<Long, List<MarketEntry>> marketEntriesByStationId;

    //    private Galaxy(List<Rare> rares, Map<Long, Commodity> commoditiesById, Map<Long, StarSystem> starSystemsById, Map<Long, Station> stationsById, Map<Long, List<MarketEntry>> marketEntriesByStationId) {
    //        this.rares = Collections.unmodifiableList(rares);
    //        this.commoditiesById = Collections.unmodifiableMap(commoditiesById);
    //        this.starSystemsById = Collections.unmodifiableMap(starSystemsById);
    //        this.stationsById = Collections.unmodifiableMap(stationsById);
    //        this.stationsByStarSystemId = Collections.unmodifiableMap(stationsById.values().stream().collect(Collectors.groupingBy(Station::getStarSystemId)));
    //        this.marketEntriesByStationId = Collections.unmodifiableMap(marketEntriesByStationId);
    //    }
    private Galaxy(Map<Long, StarSystem> starSystemsById) {
        this.starSystemsById = Collections.unmodifiableMap(starSystemsById);
    }

    public static Galaxy readDataFromFiles() throws IOException {
        final Gson gson = new GsonBuilder().registerTypeAdapter(Date.class, new SecondsSinceEpochDeserializer()).registerTypeAdapter(Boolean.class, new BooleanDigitDeserializer()).setDateFormat("yyyy-MM-dd HH:mm:ss").serializeNulls().setPrettyPrinting()
                .create();

        logger.info("Loading data...");
        long start = System.currentTimeMillis();
        //        List<Rare> rares = readRares();
        //        Map<Long, Commodity> commoditiesById = readCommodities(gson);
        Map<Long, StarSystem> starSystemsById = readStarSystems(gson);
        //        Map<Long, Station> stationsById = readStations(gson, starSystemsById);
        //        Map<Long, List<MarketEntry>> marketEntriesByStationId = readMarketEntries(stationsById, commoditiesById);
        //        setSoldRaresForStations(rares, commoditiesById.values(), starSystemsById.values(), stationsByStarSystemId);
        long end = System.currentTimeMillis();
        logger.info("Loaded data in " + (end - start) + " ms");

        //        return new Galaxy(rares, commoditiesById, starSystemsById, stationsById, marketEntriesByStationId);
        return new Galaxy(starSystemsById);
    }

    //    private static List<Rare> readRares() throws IOException {
    //        logger.debug("Loading rares...");
    //        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(RARES_FILE), "UTF-8"))) {
    //            List<Rare> rares = new ArrayList<>();
    //            Iterable<CSVRecord> records = CSVFormat.newFormat(';').withHeader().parse(reader);
    //            for (CSVRecord record : records) {
    //                rares.add(new Rare(record));
    //            }
    //            return rares;
    //        }
    //    }

    //    private static Map<Long, Commodity> readCommodities(Gson gson) throws IOException {
    //        logger.debug("Loading commodities...");
    //        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(COMMODITIES_FILE), "UTF-8"))) {
    //            List<Commodity> commodities = Arrays.asList(gson.fromJson(reader, Commodity[].class));
    //            return commodities.stream().collect(Collectors.toMap(Commodity::getId, Function.identity()));
    //        }
    //    }

    private static Map<Long, StarSystem> readStarSystems(Gson gson) throws IOException {
        logger.debug("Loading star systems...");
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(Constants.SYSTEMS_FILE), "UTF-8"))) {
            List<StarSystem> starSystems = Arrays.asList(gson.fromJson(reader, StarSystem[].class));
            starSystems.forEach(o -> o.setCoord(new Coord(o.getX(), o.getY(), o.getZ())));
            return starSystems.stream().collect(Collectors.toMap(StarSystem::getId, Function.identity()));
        }
    }

    //    private static Map<Long, Station> readStations(Gson gson, Map<Long, StarSystem> starSystems) throws IOException {
    //        logger.debug("Loading stations...");
    //        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(STATIONS_FILE), "UTF-8"))) {
    //            List<Station> stations = Arrays.asList(gson.fromJson(reader, Station[].class));
    //            stations.forEach(o -> o.setStarSystem(starSystems.get(o.getStarSystemId())));
    //            return stations.stream().collect(Collectors.toMap(Station::getId, Function.identity()));
    //        }
    //    }

    //    private static Map<Long, List<MarketEntry>> readMarketEntries(Map<Long, Station> stations, Map<Long, Commodity> commodities) throws IOException {
    //        logger.debug("Loading market entries...");
    //        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(LISTINGS_FILE), "UTF-8"))) {
    //            List<MarketEntry> marketEntries = new ArrayList<>();
    //            Iterable<CSVRecord> records = CSVFormat.DEFAULT.withHeader().parse(reader);
    //            for (CSVRecord record : records) {
    //                marketEntries.add(new MarketEntry(record, stations, commodities));
    //            }
    //            return marketEntries.stream().collect(Collectors.groupingBy(MarketEntry::getStationId));
    //        }
    //    }

    //    public List<Rare> getRares() {
    //        return this.rares;
    //    }

    //    public Map<Long, Commodity> getCommoditiesById() {
    //        return this.commoditiesById;
    //    }

    public Map<Long, StarSystem> getStarSystemsById() {
        return this.starSystemsById;
    }

    //    public Map<Long, Station> getStationsById() {
    //        return this.stationsById;
    //    }

    //    public Map<Long, List<Station>> getStationsByStarSystemId() {
    //        return this.stationsByStarSystemId;
    //    }

    //    public Map<Long, List<MarketEntry>> getMarketEntriesByStationId() {
    //        return this.marketEntriesByStationId;
    //    }

}
