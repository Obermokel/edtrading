package borg.edtrading.google;

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.sheets.v4.Sheets;
import com.google.api.services.sheets.v4.Sheets.Spreadsheets.Get;
import com.google.api.services.sheets.v4.SheetsScopes;
import com.google.api.services.sheets.v4.model.GridData;
import com.google.api.services.sheets.v4.model.Sheet;
import com.google.api.services.sheets.v4.model.Spreadsheet;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * GoogleSpreadsheet
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GoogleSpreadsheet {

    static final Logger logger = LogManager.getLogger(GoogleSpreadsheet.class);

    /** Global instance of the JSON factory. */
    private static JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();

    /** Global instance of the HTTP transport. */
    private static HttpTransport HTTP_TRANSPORT;

    static {
        try {
            HTTP_TRANSPORT = GoogleNetHttpTransport.newTrustedTransport();
        } catch (Throwable t) {
            t.printStackTrace();
            System.exit(1);
        }
    }

    private final Sheets sheetsService;
    private final String spreadsheetId;
    private final List<GoogleTable> googleTables;

    public GoogleSpreadsheet(String spreadsheetId) throws IOException {
        this(spreadsheetId, null);
    }

    public GoogleSpreadsheet(String spreadsheetId, String tableName) throws IOException {
        this.sheetsService = this.createSheetsService();
        this.spreadsheetId = spreadsheetId;
        this.googleTables = this.readTables(StringUtils.isEmpty(tableName) ? null : Arrays.asList(tableName));
    }

    private Sheets createSheetsService() throws IOException {
        logger.debug("Creating service");
        InputStream in = GoogleSpreadsheet.class.getResourceAsStream("/My Project-90e11e4da361.json");
        GoogleCredential credential = GoogleCredential.fromStream(in).createScoped(Collections.singleton(SheetsScopes.SPREADSHEETS));
        return new Sheets.Builder(HTTP_TRANSPORT, JSON_FACTORY, credential).setApplicationName("GPL Faction Scanner").build();
    }

    private List<GoogleTable> readTables(List<String> ranges) throws IOException {
        logger.debug("Reading tables");
        List<GoogleTable> tables = new ArrayList<>();
        //Spreadsheet spreadsheet = this.sheetsService.spreadsheets().get(this.spreadsheetId).setIncludeGridData(true).execute();
        Get get = this.sheetsService.spreadsheets().get(this.spreadsheetId).setIncludeGridData(true);
        if (ranges != null && !ranges.isEmpty()) {
            get.setRanges(ranges);
        }
        Spreadsheet spreadsheet = get.execute();
        List<Sheet> sheets = spreadsheet.getSheets();
        for (Sheet sheet : sheets) {
            String title = sheet.getProperties().getTitle();
            List<GridData> data = sheet.getData();
            if (data == null || data.isEmpty()) {
                logger.warn("Table '" + title + "' has no data");
            } else if (data.size() > 1) {
                logger.warn("Table '" + title + "' has " + data.size() + " data grids (expected exactly 1)");
            } else {
                logger.debug("Reading table '" + title + "'");
                try {
                    GoogleTable table = new GoogleTable(this.sheetsService, this.spreadsheetId, title, data.get(0));
                    tables.add(table);
                } catch (Exception e) {
                    logger.error("Failed to read table '" + title + "'", e);
                }
            }
        }
        return tables;
    }

    public List<GoogleTable> getTables() {
        return this.googleTables;
    }

    public GoogleTable getTable(String title) {
        for (GoogleTable googleTable : this.googleTables) {
            if (googleTable.getTitle().equals(title)) {
                return googleTable;
            }
        }
        return null;
    }

}
