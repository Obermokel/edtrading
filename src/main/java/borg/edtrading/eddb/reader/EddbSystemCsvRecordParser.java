package borg.edtrading.eddb.reader;

import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.data.EdsmSystem;
import org.apache.commons.csv.CSVRecord;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Map;

/**
 * EddbSystemCsvRecordParser
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbSystemCsvRecordParser implements CSVRecordParser<EddbSystem> {

    static final Logger logger = LogManager.getLogger(EddbSystemCsvRecordParser.class);

    private final Map<Long, EdsmSystem> edsmSystemsById;

    public EddbSystemCsvRecordParser(Map<Long, EdsmSystem> edsmSystemsById) {
        this.edsmSystemsById = edsmSystemsById;
    }

    @Override
    public EddbSystem parse(CSVRecord record) {
        EddbSystem result = new EddbSystem();

        result.setId(CSVHelper.getAsLong(record.get("id")));
        result.setEdsmId(CSVHelper.getAsLong(record.get("edsm_id")));
        result.setUpdatedAt(CSVHelper.getAsDate(record.get("updated_at")));
        result.setName(CSVHelper.getAsString(record.get("name")));
        result.setSimbadRef(CSVHelper.getAsString(record.get("simbad_ref")));
        result.setX(CSVHelper.getAsFloat(record.get("x")));
        result.setY(CSVHelper.getAsFloat(record.get("y")));
        result.setZ(CSVHelper.getAsFloat(record.get("z")));
        result.setPopulation(CSVHelper.getAsBigDecimal(record.get("population")));
        result.setIsPopulated(CSVHelper.getAsBoolean(record.get("is_populated")));
        result.setGovernmentId(CSVHelper.getAsLong(record.get("government_id")));
        result.setGovernment(CSVHelper.getAsString(record.get("government")));
        result.setAllegianceId(CSVHelper.getAsLong(record.get("allegiance_id")));
        result.setAllegiance(CSVHelper.getAsString(record.get("allegiance")));
        result.setStateId(CSVHelper.getAsLong(record.get("state_id")));
        result.setState(CSVHelper.getAsString(record.get("state")));
        result.setSecurityId(CSVHelper.getAsLong(record.get("security_id")));
        result.setSecurity(CSVHelper.getAsString(record.get("security")));
        result.setPrimaryEconomyId(CSVHelper.getAsLong(record.get("primary_economy_id")));
        result.setPrimaryEconomy(CSVHelper.getAsString(record.get("primary_economy")));
        result.setReserveTypeId(CSVHelper.getAsLong(record.get("reserve_type_id")));
        result.setReserveType(CSVHelper.getAsString(record.get("reserve_type")));
        result.setPower(CSVHelper.getAsString(record.get("power")));
        result.setPowerState(CSVHelper.getAsString(record.get("power_state")));
        result.setNeedsPermit(CSVHelper.getAsBoolean(record.get("needs_permit")));
        result.setControllingMinorFactionId(CSVHelper.getAsLong(record.get("controlling_minor_faction_id")));
        result.setControllingMinorFaction(CSVHelper.getAsString(record.get("controlling_minor_faction")));

        EdsmSystem edsmSystem = this.edsmSystemsById.get(result.getEdsmId());
        result.setCreatedAt(edsmSystem == null ? result.getUpdatedAt() : edsmSystem.getCreatedAt());

        return result;
    }

}
