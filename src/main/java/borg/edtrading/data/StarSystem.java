package borg.edtrading.data;

import borg.edtrading.util.MiscUtil;
import com.google.gson.annotations.SerializedName;
import org.apache.commons.csv.CSVRecord;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.elasticsearch.common.xcontent.XContentFactory;

import java.io.IOException;
import java.io.Serializable;
import java.util.Map;

/**
 * StarSystem
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class StarSystem implements Serializable {

    private static final long serialVersionUID = 9060200802576806089L;

    static final Logger logger = LogManager.getLogger(StarSystem.class);

    public static final String ES_TYPE = "star-system";

    private Long id = null;
    private String name = null;
    private Double x = null;
    private Double y = null;
    private Double z = null;
    private Coord coord = null;
    private Long population = null;
    @SerializedName("primary_economy")
    private String primaryEconomy = null;
    @SerializedName("needs_permit")
    private Boolean needsPermit = null;

    public StarSystem() {
        // Default
    }

    public StarSystem(CSVRecord record) {
        this.setId(MiscUtil.getAsLong(record.get("id")));
        this.setName(MiscUtil.getAsString(record.get("name")));
        this.setX(MiscUtil.getAsDouble(record.get("x")));
        this.setY(MiscUtil.getAsDouble(record.get("y")));
        this.setZ(MiscUtil.getAsDouble(record.get("z")));
        this.setPopulation(MiscUtil.getAsLong(record.get("population")));
        this.setPrimaryEconomy(MiscUtil.getAsString(record.get("primary_economy")));
        this.setNeedsPermit(MiscUtil.getAsBoolean(record.get("needs_permit")));
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
        StarSystem other = (StarSystem) obj;
        if (this.id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!this.id.equals(other.id)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
        return result;
    }

    @Override
    public String toString() {
        return String.format("#%d %s (%s)", this.getId(), this.getName(), this.getCoord());
    }

    public double distanceTo(StarSystem other) {
        return this.getCoord().distanceTo(other.getCoord());
    }

    public double distanceManhattanTo(StarSystem other) {
        return this.getCoord().distanceManhattanTo(other.getCoord());
    }

    public Long getId() {
        return this.id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Double getX() {
        return this.x;
    }

    public void setX(Double x) {
        this.x = x;
    }

    public Double getY() {
        return this.y;
    }

    public void setY(Double y) {
        this.y = y;
    }

    public Double getZ() {
        return this.z;
    }

    public void setZ(Double z) {
        this.z = z;
    }

    public Coord getCoord() {
        return this.coord;
    }

    public void setCoord(Coord coord) {
        this.coord = coord;
    }

    public Long getPopulation() {
        return this.population;
    }

    public void setPopulation(Long population) {
        this.population = population;
    }

    public String getPrimaryEconomy() {
        return this.primaryEconomy;
    }

    public void setPrimaryEconomy(String primaryEconomy) {
        this.primaryEconomy = primaryEconomy;
    }

    public Boolean getNeedsPermit() {
        return this.needsPermit;
    }

    public void setNeedsPermit(Boolean needsPermit) {
        this.needsPermit = needsPermit;
    }

    // ==== ELASTIC SEARCH ====

    public static XContentBuilder createElasticSearchMapping() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject();

            //@formatter:off
            builder
                .field("dynamic", "strict")
                .startObject("properties")
                    .startObject("name").field("type", "string").field("analyzer", "lowercaseKeyword").endObject()
                .endObject(); // END properties
            //@formatter:on

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch mapping", e);
        }
    }

    public static StarSystem fromElasticSearchSource(Map<String, Object> source) {
        StarSystem object = new StarSystem();

        object.setName(MiscUtil.getAsString(source.get("name")));

        return object;
    }

    public XContentBuilder toElasticSearchSource() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject();

            builder.field("name", this.getName());

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch source for " + this, e);
        }
    }

    public String getElasticSearchId() {
        return this.getName().toLowerCase();
    }

}
