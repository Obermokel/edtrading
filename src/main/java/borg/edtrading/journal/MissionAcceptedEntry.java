package borg.edtrading.journal;

import java.util.Date;
import java.util.LinkedHashMap;

/**
 * MissionAcceptedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MissionAcceptedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -111427685062410519L;

    private final String faction;
    private final String name;
    private final Date expiry;
    private final Integer passengerCount;
    private final Boolean passengerVIPs;
    private final Boolean passengerWanted;
    private final String passengerType;
    private final Integer missionID;
    private final String destinationSystem;
    private final String destinationStation;
    private final String commodity;
    private final String commodityLocalized;
    private final Integer count;

    public MissionAcceptedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.faction = this.readString(data, "Faction");
        this.name = this.readString(data, "Name");
        this.expiry = this.readDate(data, "Expiry");
        this.passengerCount = this.readInt(data, "PassengerCount");
        this.passengerVIPs = this.readBoolean(data, "PassengerVIPs");
        this.passengerWanted = this.readBoolean(data, "PassengerWanted");
        this.passengerType = this.readString(data, "PassengerType");
        this.missionID = this.readInt(data, "MissionID");
        this.destinationSystem = this.readString(data, "DestinationSystem");
        this.destinationStation = this.readString(data, "DestinationStation");
        this.commodity = this.readString(data, "Commodity");
        this.commodityLocalized = this.readString(data, "Commodity_Localised");
        this.count = this.readInt(data, "Count");
    }

    public String getFaction() {
        return this.faction;
    }

    public String getName() {
        return this.name;
    }

    public Date getExpiry() {
        return this.expiry;
    }

    public Integer getPassengerCount() {
        return this.passengerCount;
    }

    public Boolean getPassengerVIPs() {
        return this.passengerVIPs;
    }

    public Boolean getPassengerWanted() {
        return this.passengerWanted;
    }

    public String getPassengerType() {
        return this.passengerType;
    }

    public Integer getMissionID() {
        return this.missionID;
    }

    public String getDestinationSystem() {
        return this.destinationSystem;
    }

    public String getDestinationStation() {
        return this.destinationStation;
    }

    public String getCommodity() {
        return this.commodity;
    }

    public String getCommodityLocalized() {
        return this.commodityLocalized;
    }

    public Integer getCount() {
        return this.count;
    }

}
