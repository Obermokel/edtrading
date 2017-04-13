package borg.edtrading.google;

import com.google.api.services.sheets.v4.Sheets;
import com.google.api.services.sheets.v4.Sheets.Spreadsheets.Values.Update;
import com.google.api.services.sheets.v4.model.CellData;
import com.google.api.services.sheets.v4.model.GridData;
import com.google.api.services.sheets.v4.model.RowData;
import com.google.api.services.sheets.v4.model.ValueRange;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * GoogleTable
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GoogleTable {

    static final Logger logger = LogManager.getLogger(GoogleTable.class);

    private final Sheets sheetsService;
    private final String spreadsheetId;
    private final String title;
    private final List<List<String>> data = new ArrayList<>(); // rows, then cols

    public GoogleTable(Sheets sheetsService, String spreadsheetId, String title, GridData gridData) {
        this.sheetsService = sheetsService;
        this.spreadsheetId = spreadsheetId;
        this.title = title;

        this.readGridData(gridData);
    }

    private void readGridData(GridData gridData) {
        List<RowData> rowData = gridData.getRowData();
        if (rowData != null) {
            for (int rowIdx = 0; rowIdx < rowData.size(); rowIdx++) {
                RowData rd = rowData.get(rowIdx);
                if (rd != null) {
                    List<CellData> cellData = rd.getValues();
                    if (cellData != null) {
                        for (int colIdx = 0; colIdx < cellData.size(); colIdx++) {
                            CellData cd = cellData.get(colIdx);
                            if (cd != null && cd.getFormattedValue() != null) {
                                this.initCellValue(rowIdx, colIdx, cd.getFormattedValue());
                            }
                        }
                    }
                }
            }
        }
    }

    private void initCellValue(int rowIdx, int colIdx, String value) {
        while (this.data.size() <= rowIdx) {
            this.data.add(new ArrayList<>(0));
        }
        List<String> row = this.data.get(rowIdx);
        while (row.size() <= colIdx) {
            row.add(null);
        }
        row.set(colIdx, value);
    }

    public String getTitle() {
        return this.title;
    }

    public List<List<String>> getData() {
        return this.data;
    }

    public int getColumnIndex(String headline) {
        if (!this.data.isEmpty()) {
            List<String> headlineValues = this.data.get(0);
            for (int colIdx = 0; colIdx < headlineValues.size(); colIdx++) {
                if (headlineValues.get(colIdx) != null && headlineValues.get(colIdx).equals(headline)) {
                    return colIdx;
                }
            }
        }
        return -1;
    }

    /**
     * @return Column index of the new column
     */
    public int addColumn(String headline) throws IOException {
        if (StringUtils.isBlank(headline)) {
            throw new IllegalArgumentException("headline must not be blank");
        }

        try {
            int colIdx = this.data.isEmpty() ? 0 : this.data.get(0).size();
            String range = this.title + "!" + colIdxToLetter(colIdx) + "1";
            ValueRange content = new ValueRange();
            content.setValues(Arrays.asList(Arrays.asList(headline)));
            Update update = this.sheetsService.spreadsheets().values().update(this.spreadsheetId, range, content);
            update.setValueInputOption("USER_ENTERED");
            Integer updatedCells = update.execute().getUpdatedCells();
            if (updatedCells == null || updatedCells.intValue() != 1) {
                throw new RuntimeException("Expected 1 updated cell, but was " + updatedCells);
            } else {
                if (this.data.isEmpty()) {
                    this.data.add(new ArrayList<>());
                }
                List<String> headlineValues = this.data.get(0);
                headlineValues.add(headline);
                return colIdx;
            }
        } catch (Exception e) {
            throw new IOException("Failed to add column '" + headline + "'", e);
        }
    }

    public int getRowIndex(String headline) {
        if (!this.data.isEmpty()) {
            for (int rowIdx = 0; rowIdx < this.data.size(); rowIdx++) {
                List<String> row = this.data.get(rowIdx);
                if (row != null && row.size() > 0 && row.get(0) != null && row.get(0).equals(headline)) {
                    return rowIdx;
                }
            }
        }
        return -1;
    }

    /**
     * @return Row index of the new row
     */
    public int addRow(String headline) throws IOException {
        if (StringUtils.isBlank(headline)) {
            throw new IllegalArgumentException("headline must not be blank");
        }

        try {
            int rowIdx = this.data.isEmpty() ? 0 : this.data.size();
            String range = this.title + "!A" + (rowIdx + 1);
            ValueRange content = new ValueRange();
            content.setValues(Arrays.asList(Arrays.asList(headline)));
            Update update = this.sheetsService.spreadsheets().values().update(this.spreadsheetId, range, content);
            update.setValueInputOption("USER_ENTERED");
            Integer updatedCells = update.execute().getUpdatedCells();
            if (updatedCells == null || updatedCells.intValue() != 1) {
                throw new RuntimeException("Expected 1 updated cell, but was " + updatedCells);
            } else {
                List<String> row = new ArrayList<>();
                row.add(headline);
                this.data.add(row);
                return rowIdx;
            }
        } catch (Exception e) {
            throw new IOException("Failed to add row '" + headline + "'", e);
        }
    }

    public static String colIdxToLetter(int colIdx) {
        return Character.toString("ABCDEFGHIJKLMNOPQRSTUVWXYZ".charAt(colIdx));
    }

    public String getCellValue(int rowIdx, int colIdx) {
        try {
            return this.data.get(rowIdx).get(colIdx);
        } catch (Exception e) {
            return null;
        }
    }

    public void setCellValue(int rowIdx, int colIdx, String value) throws IOException {
        String range = this.title + "!" + colIdxToLetter(colIdx) + (rowIdx + 1);
        try {
            ValueRange content = new ValueRange();
            content.setValues(Arrays.asList(Arrays.asList(value)));
            Update update = this.sheetsService.spreadsheets().values().update(this.spreadsheetId, range, content);
            update.setValueInputOption("USER_ENTERED");
            Integer updatedCells = update.execute().getUpdatedCells();
            if (updatedCells == null || updatedCells.intValue() != 1) {
                throw new RuntimeException("Expected 1 updated cell, but was " + updatedCells);
            } else {
                this.initCellValue(rowIdx, colIdx, value);
            }
        } catch (Exception e) {
            throw new IOException("Failed to set " + range + " to '" + value + "'", e);
        }
    }

    public void insertRowAbove(int rowIdx) {
        // TODO
    }

}
