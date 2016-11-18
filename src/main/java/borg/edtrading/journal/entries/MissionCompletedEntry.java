package borg.edtrading.journal.entries;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.NameCount;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * MissionCompletedEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MissionCompletedEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 6658818872365212879L;

    private final String faction;
    private final String name;
    private final Integer missionID;
    private final String destinationSystem;
    private final String destinationStation;
    private final Integer reward;
    private final String commodity;
    private final String commodityLocalized;
    private final Integer count;
    private final Integer donation;
    private final List<NameCount> commodityReward;

    public MissionCompletedEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.faction = this.readString(data, "Faction");
        this.name = this.readString(data, "Name");
        this.missionID = this.readInt(data, "MissionID");
        this.destinationSystem = this.readString(data, "DestinationSystem");
        this.destinationStation = this.readString(data, "DestinationStation");
        this.reward = this.readInt(data, "Reward");
        this.commodity = this.readString(data, "Commodity");
        this.commodityLocalized = this.readString(data, "Commodity_Localised");
        this.count = this.readInt(data, "Count");
        this.donation = this.readInt(data, "Donation");
        this.commodityReward = this.readNameCountList(data, "CommodityReward");
    }

    public String getFaction() {
        return this.faction;
    }

    public String getName() {
        return this.name;
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

    public Integer getReward() {
        return this.reward;
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

    public Integer getDonation() {
        return this.donation;
    }

    public List<NameCount> getCommodityReward() {
        return this.commodityReward;
    }

}
