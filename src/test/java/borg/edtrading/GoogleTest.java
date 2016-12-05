package borg.edtrading;

import com.google.api.client.auth.oauth2.Credential;
import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.sheets.v4.Sheets;
import com.google.api.services.sheets.v4.Sheets.Spreadsheets.Values.Append;
import com.google.api.services.sheets.v4.SheetsScopes;
import com.google.api.services.sheets.v4.model.AppendValuesResponse;
import com.google.api.services.sheets.v4.model.ValueRange;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * GoogleTest
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GoogleTest {

    static final Logger logger = LogManager.getLogger(GoogleTest.class);

    /** Application name. */
    private static final String APPLICATION_NAME = "Google Sheets API Java Quickstart";

    /** Global instance of the JSON factory. */
    private static final JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();

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

    /**
     * Creates an authorized Credential object.
     * @return an authorized Credential object.
     * @throws IOException
     */
    public static Credential authorize() throws IOException {
        InputStream in = GoogleTest.class.getResourceAsStream("/My Project-90e11e4da361.json");
        GoogleCredential credential = GoogleCredential.fromStream(in).createScoped(Collections.singleton(SheetsScopes.SPREADSHEETS));
        return credential;
    }

    /**
     * Build and return an authorized Sheets API client service.
     * @return an authorized Sheets API client service
     * @throws IOException
     */
    public static Sheets getSheetsService() throws IOException {
        Credential credential = authorize();
        return new Sheets.Builder(HTTP_TRANSPORT, JSON_FACTORY, credential).setApplicationName(APPLICATION_NAME).build();
    }

    public static void main(String[] args) throws IOException {
        // Build a new authorized API client service.
        Sheets service = getSheetsService();

        // Prints the names and majors of students in a sample spreadsheet:
        // https://docs.google.com/spreadsheets/d/1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms/edit
        String spreadsheetId = "1RFqFgOrwyqsvFG5hFNzV1jek0CzZ7SHwrT3bQFhKx6U";
        //        String range = "rawdata!A1:B";
        //        ValueRange response = spreadsheets.values().get(spreadsheetId, range).execute();
        //        List<List<Object>> values = response.getValues();
        //        if (values == null || values.size() == 0) {
        //            System.out.println("No data found.");
        //        } else {
        //            System.out.println("Faction, Influence");
        //            for (List row : values) {
        //                // Print columns A and E, which correspond to indices 0 and 4.
        //                System.out.printf("%s, %s\n", row.get(0), row.get(1));
        //            }
        //        }

        ValueRange vr = new ValueRange();
        List<List<Object>> rows = new ArrayList<>();
        List<Object> row = new ArrayList<>();
        row.add("09.12.2016");
        row.add(88);
        row.add(12);
        rows.add(row);
        vr.setValues(rows);
        Append append = service.spreadsheets().values().append(spreadsheetId, "RAWDATA_Maridal!A:C", vr);
        append.setValueInputOption("USER_ENTERED");
        AppendValuesResponse appendResponse = append.execute();
        System.out.println(appendResponse.toPrettyString());
    }

}
