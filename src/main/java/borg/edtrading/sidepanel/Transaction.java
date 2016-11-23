package borg.edtrading.sidepanel;

import borg.edtrading.journal.entries.missions.CommunityGoalJoinEntry;
import borg.edtrading.journal.entries.missions.CommunityGoalRewardEntry;
import borg.edtrading.journal.entries.missions.MissionAcceptedEntry;
import borg.edtrading.journal.entries.missions.MissionCompletedEntry;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * Transaction
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Transaction implements Serializable {

    private static final long serialVersionUID = 3434953144997448904L;

    static final Logger logger = LogManager.getLogger(Transaction.class);

    private TransactionType type = null;
    private Date timestamp = null;
    private String name = null;
    private String faction = null; // Who gave the mission / Who pays the bounty voucher or combat bond / Who put the bounty or fine on us

    private Integer missionID = null;
    private Date missionExpiryDate = null;

    private String destinationSystem = null;
    private String destinationStation = null;

    private String commodity = null;
    private String commodityLocalized = null;
    private Integer commodityCount = null;
    private String passengerType = null;
    private Integer passengerCount = null;

    private int credits = 0; // Mission reward (unfortunately not known) / Bounty voucher or combat bond / Bounty or fine

    public Transaction() {
        // Default
    }

    public Transaction(MissionAcceptedEntry e) {
        this.setType(TransactionType.MISSION);
        this.setTimestamp(e.getTimestamp());
        this.setName(e.getName());
        this.setFaction(e.getFaction());

        this.setMissionID(e.getMissionID());
        this.setMissionExpiryDate(e.getExpiry());

        this.setDestinationSystem(e.getDestinationSystem());
        this.setDestinationStation(e.getDestinationStation());

        this.setCommodity(e.getCommodity());
        this.setCommodityLocalized(e.getCommodityLocalized());
        this.setCommodityCount(e.getCount());
        this.setPassengerType(e.getPassengerType());
        this.setPassengerCount(e.getPassengerCount());

        this.setCredits(0); // :-(
    }

    public Transaction(MissionCompletedEntry e) {
        this.setType(TransactionType.MISSION);
        this.setTimestamp(e.getTimestamp());
        this.setName(e.getName());
        this.setFaction(e.getFaction());

        this.setMissionID(e.getMissionID());

        this.setDestinationSystem(e.getDestinationSystem());
        this.setDestinationStation(e.getDestinationStation());

        this.setCommodity(e.getCommodity());
        this.setCommodityLocalized(e.getCommodityLocalized());
        this.setCommodityCount(e.getCount());

        this.setCredits(MiscUtil.getAsInt(e.getReward(), 0));
    }

    public Transaction(CommunityGoalJoinEntry e) {
        this.setType(TransactionType.COMMUNITY_GOAL);
        this.setTimestamp(e.getTimestamp());
        this.setName(e.getName());

        this.setDestinationSystem(e.getSystem());

        this.setCredits(0); // :-(
    }

    public Transaction(CommunityGoalRewardEntry e) {
        this.setType(TransactionType.COMMUNITY_GOAL);
        this.setTimestamp(e.getTimestamp());
        this.setName(e.getName());

        this.setDestinationSystem(e.getSystem());

        this.setCredits(MiscUtil.getAsInt(e.getReward(), 0));
    }

    @Override
    public String toString() {
        return String.format(Locale.US, "%s [%s] %s", this.getType(), new SimpleDateFormat("yyyy-MM-dd HH:mm").format(this.getTimestamp()), this.getName());
    }

    public TransactionType getType() {
        return this.type;
    }

    public void setType(TransactionType type) {
        this.type = type;
    }

    public Date getTimestamp() {
        return this.timestamp;
    }

    public void setTimestamp(Date timestamp) {
        this.timestamp = timestamp;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getFaction() {
        return this.faction;
    }

    public void setFaction(String faction) {
        this.faction = faction;
    }

    public Integer getMissionID() {
        return this.missionID;
    }

    public void setMissionID(Integer missionID) {
        this.missionID = missionID;
    }

    public Date getMissionExpiryDate() {
        return this.missionExpiryDate;
    }

    public void setMissionExpiryDate(Date missionExpiryDate) {
        this.missionExpiryDate = missionExpiryDate;
    }

    public String getDestinationSystem() {
        return this.destinationSystem;
    }

    public void setDestinationSystem(String destinationSystem) {
        this.destinationSystem = destinationSystem;
    }

    public String getDestinationStation() {
        return this.destinationStation;
    }

    public void setDestinationStation(String destinationStation) {
        this.destinationStation = destinationStation;
    }

    public String getCommodity() {
        return this.commodity;
    }

    public void setCommodity(String commodity) {
        this.commodity = commodity;
    }

    public String getCommodityLocalized() {
        return this.commodityLocalized;
    }

    public void setCommodityLocalized(String commodityLocalized) {
        this.commodityLocalized = commodityLocalized;
    }

    public Integer getCommodityCount() {
        return this.commodityCount;
    }

    public void setCommodityCount(Integer commodityCount) {
        this.commodityCount = commodityCount;
    }

    public String getPassengerType() {
        return this.passengerType;
    }

    public void setPassengerType(String passengerType) {
        this.passengerType = passengerType;
    }

    public Integer getPassengerCount() {
        return this.passengerCount;
    }

    public void setPassengerCount(Integer passengerCount) {
        this.passengerCount = passengerCount;
    }

    public int getCredits() {
        return this.credits;
    }

    public void setCredits(int credits) {
        this.credits = credits;
    }

    public static enum TransactionType {
        COMMUNITY_GOAL, MISSION, VOUCHER, BOUNTY, FINE;
    }

}
