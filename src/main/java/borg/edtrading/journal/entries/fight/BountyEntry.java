package borg.edtrading.journal.entries.fight;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.util.MiscUtil;

import java.io.Serializable;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * BountyEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BountyEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = -2858103216807639804L;

    private final String victimFaction;
    private final Integer totalReward;
    private final List<BountyReward> rewards;
    private final Integer sharedWithOthers;
    private final String target;
    private final String faction;
    private final String factionLocalized;
    private final Integer reward;

    public BountyEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.victimFaction = this.readString(data, "VictimFaction");
        this.totalReward = this.readInt(data, "TotalReward");
        this.rewards = this.readRewards(data, "Rewards");
        this.sharedWithOthers = this.readInt(data, "SharedWithOthers");
        this.target = this.readString(data, "Target");
        this.faction = this.readString(data, "Faction");
        this.factionLocalized = this.readString(data, "Faction_Localised");
        this.reward = this.readInt(data, "Reward");
    }

    private List<BountyReward> readRewards(LinkedHashMap<String, Object> data, String name) {
        List<Map> list = this.readList(data, name, Map.class);

        if (list == null) {
            return null;
        } else {
            return list.stream().map(m -> new BountyReward(MiscUtil.getAsString(m.get("Faction")), MiscUtil.getAsInt(m.get("Reward")))).collect(Collectors.toList());
        }
    }

    public String getVictimFaction() {
        return this.victimFaction;
    }

    public Integer getTotalReward() {
        return this.totalReward;
    }

    public List<BountyReward> getRewards() {
        return this.rewards;
    }

    public Integer getSharedWithOthers() {
        return this.sharedWithOthers;
    }

    public String getTarget() {
        return this.target;
    }

    public String getFaction() {
        return this.faction;
    }

    public String getFactionLocalized() {
        return this.factionLocalized;
    }

    public Integer getReward() {
        return this.reward;
    }

    public static class BountyReward implements Serializable {

        private static final long serialVersionUID = -6381732944430644313L;

        private String faction = null;
        private Integer reward = null;

        public BountyReward() {
            // Default
        }

        public BountyReward(String faction, Integer reward) {
            this.setFaction(faction);
            this.setReward(reward);
        }

        public String getFaction() {
            return this.faction;
        }

        public void setFaction(String faction) {
            this.faction = faction;
        }

        public Integer getReward() {
            return this.reward;
        }

        public void setReward(Integer reward) {
            this.reward = reward;
        }

    }

}
