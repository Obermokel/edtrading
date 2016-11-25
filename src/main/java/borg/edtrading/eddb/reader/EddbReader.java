package borg.edtrading.eddb.reader;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbCommodity;
import borg.edtrading.eddb.data.EddbData;
import borg.edtrading.eddb.data.EddbEntity;
import borg.edtrading.eddb.data.EddbFaction;
import borg.edtrading.eddb.data.EddbMarketEntry;
import borg.edtrading.eddb.data.EddbModule;
import borg.edtrading.eddb.data.EddbStation;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.json.BooleanDigitDeserializer;
import borg.edtrading.json.SecondsSinceEpochDeserializer;
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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.zip.GZIPInputStream;

/**
 * EddbReader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbReader {

    static final Logger logger = LogManager.getLogger(EddbReader.class);

    public static void main(String[] args) throws IOException {
        read();
    }

    public static EddbData read() throws IOException {
        long downloadStart = System.currentTimeMillis();
        File commoditiesFile = downloadIfUpdated("https://eddb.io/archive/v5/commodities.json");
        File modulesFile = downloadIfUpdated("https://eddb.io/archive/v5/modules.json");
        File marketEntriesFile = downloadIfUpdated("https://eddb.io/archive/v5/listings.csv");
        File systemsFile = downloadIfUpdated("https://eddb.io/archive/v5/systems.csv");
        File systemsPopulatedFile = downloadIfUpdated("https://eddb.io/archive/v5/systems_populated.json");
        File bodiesFile = downloadIfUpdated("https://eddb.io/archive/v5/bodies.json");
        File stationsFile = downloadIfUpdated("https://eddb.io/archive/v5/stations.json");
        File factionsFile = downloadIfUpdated("https://eddb.io/archive/v5/factions.json");
        long downloadEnd = System.currentTimeMillis();
        logger.info("Downloaded data in " + DurationFormatUtils.formatDuration(downloadEnd - downloadStart, "H:mm:ss"));

        long readStart = System.currentTimeMillis();
        Map<Long, EddbCommodity> commoditiesById = readJsonFile(commoditiesFile, EddbCommodity.class);
        Map<Long, EddbModule> modulesById = readJsonFile(modulesFile, EddbModule.class);
        Map<Long, EddbMarketEntry> marketEntriesById = readCsvFile(marketEntriesFile, new EddbMarketEntryCsvRecordParser());
        Map<Long, EddbSystem> systemsById = readCsvFile(systemsFile, new EddbSystemCsvRecordParser());
        Map<Long, EddbSystem> systemsPopulatedById = readJsonFile(systemsPopulatedFile, EddbSystem.class);
        Map<Long, EddbBody> bodiesById = readJsonFile(bodiesFile, EddbBody.class);
        Map<Long, EddbStation> stationsById = readJsonFile(stationsFile, EddbStation.class);
        Map<Long, EddbFaction> factionsById = readJsonFile(factionsFile, EddbFaction.class);
        long readEnd = System.currentTimeMillis();
        logger.info("Read data in " + DurationFormatUtils.formatDuration(readEnd - readStart, "H:mm:ss"));

        logger.debug(String.format(Locale.US, "Raw data:\n%,11d commodities\n%,11d modules\n%,11d marketEntries\n%,11d systems\n%,11d systemsPopulated\n%,11d bodies\n%,11d stations\n%,11d factions", commoditiesById.size(), modulesById.size(),
                marketEntriesById.size(), systemsById.size(), systemsPopulatedById.size(), bodiesById.size(), stationsById.size(), factionsById.size()));

        long postprocessStart = System.currentTimeMillis();
        systemsById.values().parallelStream().forEach(system -> {
            system.setCoord(new Coord(system.getX(), system.getY(), system.getZ()));
            system.setDistanceFromSol(system.getCoord().distanceToSol());
        });
        bodiesById.values().parallelStream().forEach(body -> {
            body.setSystem(systemsById.get(body.getSystemId()));
        });
        stationsById.values().parallelStream().forEach(station -> {
            station.setSystem(systemsById.get(station.getSystemId()));
            station.setBody(bodiesById.get(station.getBodyId()));
            station.setControllingMinorFaction(factionsById.get(station.getControllingMinorFactionId()));
        });
        factionsById.values().parallelStream().forEach(faction -> {
            faction.setHomeSystem(systemsById.get(faction.getHomeSystemId()));
        });
        long postprocessEnd = System.currentTimeMillis();
        logger.info("Post-processed data in " + DurationFormatUtils.formatDuration(postprocessEnd - postprocessStart, "H:mm:ss"));

        return new EddbData(systemsById);
    }

    private static <T extends EddbEntity> Map<Long, T> readCsvFile(File file, CSVRecordParser<T> csvRecordParser) throws IOException {
        List<T> templist = new ArrayList<>();

        logger.debug("Reading " + file.getName());
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))) {
            Iterable<CSVRecord> records = CSVFormat.DEFAULT.withHeader().parse(reader);
            for (CSVRecord record : records) {
                templist.add(csvRecordParser.parse(record));
            }
        }

        Map<Long, T> map = new HashMap<>(templist.size());
        for (T t : templist) {
            map.put(t.getId(), t);
        }
        return map;
    }

    private static <T extends EddbEntity> Map<Long, T> readJsonFile(File file, Class<T> type) throws IOException {
        ArrayList<T> templist = null;

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
                templist = new ArrayList<>();
                String line = reader.readLine();
                while (line != null) {
                    try {
                        templist.add(gson.fromJson(line, type));
                    } catch (JsonSyntaxException e) {
                        logger.warn("Corrupt line in " + file + ": " + line, e);
                    }
                    line = reader.readLine();
                }
            } else {
                T[] array = (T[]) gson.fromJson(reader, Array.newInstance(type, 0).getClass());
                templist = new ArrayList<>(array.length);
                for (T t : array) {
                    templist.add(t);
                }
            }
        }

        Map<Long, T> map = new HashMap<>(templist.size());
        for (T t : templist) {
            map.put(t.getId(), t);
        }
        return map;
    }

    private static File downloadIfUpdated(String url) throws IOException {
        String filename = url.substring(url.lastIndexOf("/") + 1);

        File baseDir = new File(System.getProperty("user.home"), ".eddbdata");
        File backupDir = new File(baseDir, "backup");
        if (!backupDir.exists()) {
            backupDir.mkdirs();
        }
        File file = new File(baseDir, filename);

        if (!file.exists() || headLastModified(url) > file.lastModified()) {
            backup(file, backupDir);
            download(url, file);
        } else {
            logger.debug(filename + " is up-to-date");
        }

        return file;
    }

    private static long backup(File sourceFile, File backupDir) throws IOException {
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

    private static long download(String url, File file) throws IOException {
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

    private static long headLastModified(String url) throws IOException {
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

    private static long headContentLength(String url) throws IOException {
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
