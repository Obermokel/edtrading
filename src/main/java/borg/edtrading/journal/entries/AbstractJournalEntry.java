package borg.edtrading.journal.entries;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.ModuleData;
import borg.edtrading.journal.NameCount;
import borg.edtrading.journal.PassengerManifestData;
import borg.edtrading.journal.RingData;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * AbstractJournalEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class AbstractJournalEntry implements Serializable, Comparable<AbstractJournalEntry> {

    private static final long serialVersionUID = -9096842249098740097L;

    static final Logger logger = LogManager.getLogger(AbstractJournalEntry.class);

    private final Date timestamp;
    private final Event event;

    public AbstractJournalEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        this.timestamp = timestamp;
        this.event = event;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AbstractJournalEntry other = (AbstractJournalEntry) obj;
        if (this.event != other.event) {
            return false;
        }
        if (this.timestamp == null) {
            if (other.timestamp != null) {
                return false;
            }
        } else if (!this.timestamp.equals(other.timestamp)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.event == null) ? 0 : this.event.hashCode());
        result = prime * result + ((this.timestamp == null) ? 0 : this.timestamp.hashCode());
        return result;
    }

    @Override
    public String toString() {
        return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(this.timestamp) + ": " + this.event;
    }

    @Override
    public int compareTo(AbstractJournalEntry other) {
        return this.timestamp.compareTo(other.timestamp);
    }

    protected String readString(Map<String, Object> data, String name) {
        return data.containsKey(name) ? (String) data.remove(name) : null;
    }

    protected Boolean readBoolean(Map<String, Object> data, String name) {
        return data.containsKey(name) ? (Boolean) data.remove(name) : null;
    }

    protected Integer readInt(Map<String, Object> data, String name) {
        return data.containsKey(name) ? ((Number) data.remove(name)).intValue() : null;
    }

    protected Float readFloat(Map<String, Object> data, String name) {
        return data.containsKey(name) ? ((Number) data.remove(name)).floatValue() : null;
    }

    protected BigDecimal readBigDecimal(Map<String, Object> data, String name) {
        return data.containsKey(name) ? new BigDecimal(data.remove(name).toString()) : null;
    }

    protected Date readDate(Map<String, Object> data, String name) {
        try {
            return data.containsKey(name) ? new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'").parse(data.remove(name).toString()) : null;
        } catch (ParseException e) {
            throw new RuntimeException("Failed to parse date", e);
        }
    }

    protected <T> List<T> readList(Map<String, Object> data, String name, Class<T> type) {
        return data.containsKey(name) ? (List<T>) data.remove(name) : null;
    }

    protected List<NameCount> readNameCountList(Map<String, Object> data, String name) {
        List<Map> ncList = this.readList(data, name, Map.class);

        if (ncList != null) {
            List<NameCount> result = new ArrayList<>(ncList.size());
            for (Map m : ncList) {
                result.add(new NameCount(MiscUtil.getAsString(m.get("Name")), MiscUtil.getAsInt(m.get("Count"))));
            }
            return result;
        } else {
            return null;
        }
    }

    protected <K, V> Map<K, V> readMap(Map<String, Object> data, String name, Class<K> typeKey, Class typeValue) {
        return data.containsKey(name) ? (Map<K, V>) data.remove(name) : null;
    }

    protected Map<String, Integer> readNameCountMap(Map<String, Object> data, String name) {
        Object nc = data.remove(name);

        if (nc instanceof List) {
            List<Map<String, Object>> ncList = (List<Map<String, Object>>) nc;

            Map<String, Integer> result = new LinkedHashMap<>(ncList.size());
            for (Map<String, Object> m : ncList) {
                result.put(MiscUtil.getAsString(m.get("Name")), MiscUtil.getAsInt(m.get("Count")));
            }
            return result;
        } else if (nc instanceof Map) {
            Map<String, Number> ncMap = (Map<String, Number>) nc;

            Map<String, Integer> result = new LinkedHashMap<>(ncMap.size());
            for (String n : ncMap.keySet()) {
                result.put(n, ncMap.get(n).intValue());
            }
            return result;
        } else {
            return null;
        }
    }

    protected Coord readCoord(Map<String, Object> data, String name) {
        List<Number> xyz = this.readList(data, name, Number.class);

        if (xyz != null && xyz.size() == 3) {
            return new Coord(xyz.get(0).floatValue(), xyz.get(1).floatValue(), xyz.get(2).floatValue());
        } else {
            return null;
        }
    }

    protected List<RingData> readRings(Map<String, Object> data, String name) {
        List<Map> list = this.readList(data, name, Map.class);

        if (list == null) {
            return null;
        } else {
            List<RingData> result = new ArrayList<>(list.size());
            for (Map e : list) {
                String ringName = this.readString(e, "Name");
                String ringClass = this.readString(e, "RingClass");
                BigDecimal massMT = this.readBigDecimal(e, "MassMT");
                BigDecimal innerRad = this.readBigDecimal(e, "InnerRad");
                BigDecimal outerRad = this.readBigDecimal(e, "OuterRad");
                result.add(new RingData(ringName, ringClass, massMT, innerRad, outerRad));
            }
            return result;
        }
    }

    protected List<ModuleData> readModules(Map<String, Object> data, String name) {
        List<Map> list = this.readList(data, name, Map.class);

        if (list == null) {
            return null;
        } else {
            List<ModuleData> result = new ArrayList<>(list.size());
            for (Map e : list) {
                String slot = this.readString(e, "Slot");
                String item = this.readString(e, "Item");
                Boolean on = this.readBoolean(e, "On");
                Integer priority = this.readInt(e, "Priority");
                Float health = this.readFloat(e, "Health");
                Integer value = this.readInt(e, "Value");
                result.add(new ModuleData(slot, item, on, priority, health, value));
            }
            return result;
        }
    }

    protected List<PassengerManifestData> readPassengerManifest(Map<String, Object> data, String name) {
        List<Map> list = this.readList(data, name, Map.class);

        if (list == null) {
            return null;
        } else {
            List<PassengerManifestData> result = new ArrayList<>(list.size());
            for (Map e : list) {
                String type = this.readString(e, "Type");
                Boolean vip = this.readBoolean(e, "VIP");
                Boolean wanted = this.readBoolean(e, "Wanted");
                Integer count = this.readInt(e, "Count");
                Integer missionID = this.readInt(e, "MissionID");
                result.add(new PassengerManifestData(type, vip, wanted, count, missionID));
            }
            return result;
        }
    }

    protected Map<String, Integer> readAmounts(Map<String, Object> data, String name, String key) {
        Object nc = data.remove(name);

        if (nc instanceof List) {
            List<Map<String, Object>> ncList = (List<Map<String, Object>>) nc;

            Map<String, Integer> result = new LinkedHashMap<>(ncList.size());
            for (Map<String, Object> m : ncList) {
                result.put(MiscUtil.getAsString(m.get(key)), MiscUtil.getAsInt(m.get("Amount")));
            }
            return result;
        } else if (nc instanceof Map) {
            Map<String, Number> ncMap = (Map<String, Number>) nc;

            Map<String, Integer> result = new LinkedHashMap<>(ncMap.size());
            for (String n : ncMap.keySet()) {
                result.put(n, ncMap.get(n).intValue());
            }
            return result;
        } else {
            return null;
        }
    }

    protected Map<String, Float> readPercentages(Map<String, Object> data, String name) {
        Object nc = data.remove(name);

        if (nc instanceof List) {
            List<Map<String, Object>> ncList = (List<Map<String, Object>>) nc;

            Map<String, Float> result = new LinkedHashMap<>(ncList.size());
            for (Map<String, Object> m : ncList) {
                result.put(MiscUtil.getAsString(m.get("Name")), MiscUtil.getAsFloat(m.get("Percent")));
            }
            return result;
        } else if (nc instanceof Map) {
            Map<String, Number> ncMap = (Map<String, Number>) nc;

            Map<String, Float> result = new LinkedHashMap<>(ncMap.size());
            for (String n : ncMap.keySet()) {
                result.put(n, ncMap.get(n).floatValue());
            }
            return result;
        } else {
            return null;
        }
    }

    protected List<Faction> readFactions(LinkedHashMap<String, Object> data, String name) {
        List<Map> list = this.readList(data, name, Map.class);

        if (list == null) {
            return null;
        } else {
            List<Faction> result = new ArrayList<>();

            for (Map m : list) {
                Faction faction = new Faction();
                faction.setName((String) m.get("Name"));
                faction.setAllegiance((String) m.get("Allegiance"));
                faction.setGovernment((String) m.get("Government"));
                faction.setFactionState((String) m.get("FactionState"));
                faction.setInfluence(((Number) m.get("Influence")).floatValue());
                result.add(faction);
            }

            return result;
        }
    }

    public Date getTimestamp() {
        return this.timestamp;
    }

    public Event getEvent() {
        return this.event;
    }

    public static class Faction implements Serializable {

        private static final long serialVersionUID = 390359291506715008L;

        private String name = null;
        private String allegiance = null;
        private String government = null;
        private String factionState = null;
        private Float influence = null;

        @Override
        public String toString() {
            return this.name + " (" + this.factionState + "): " + String.format(Locale.US, "%.1f%%", this.influence * 100);
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getAllegiance() {
            return this.allegiance;
        }

        public void setAllegiance(String allegiance) {
            this.allegiance = allegiance;
        }

        public String getGovernment() {
            return this.government;
        }

        public void setGovernment(String government) {
            this.government = government;
        }

        public String getFactionState() {
            return this.factionState;
        }

        public void setFactionState(String factionState) {
            this.factionState = factionState;
        }

        public Float getInfluence() {
            return this.influence;
        }

        public void setInfluence(Float influence) {
            this.influence = influence;
        }

    }

}
