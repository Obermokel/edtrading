package borg.edtrading.eddb.reader;

import borg.edtrading.eddb.data.EddbMarketEntry;
import org.apache.commons.csv.CSVRecord;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * EddbMarketEntryCsvRecordParser
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbMarketEntryCsvRecordParser implements CSVRecordParser<EddbMarketEntry> {

    static final Logger logger = LogManager.getLogger(EddbMarketEntryCsvRecordParser.class);

    @Override
    public EddbMarketEntry parse(CSVRecord record) {
        EddbMarketEntry result = new EddbMarketEntry();

        result.setId(CSVHelper.getAsLong(record.get("id")));
        result.setStationId(CSVHelper.getAsLong(record.get("station_id")));
        result.setCommodityId(CSVHelper.getAsLong(record.get("commodity_id")));
        result.setSupply(CSVHelper.getAsInt(record.get("supply")));
        result.setBuyPrice(CSVHelper.getAsInt(record.get("buy_price")));
        result.setDemand(CSVHelper.getAsInt(record.get("demand")));
        result.setSellPrice(CSVHelper.getAsInt(record.get("sell_price")));
        result.setCollectedAt(CSVHelper.getAsDate(record.get("collected_at")));

        return result;
    }

}
