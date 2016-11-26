package borg.edtrading.eddb.data;

import borg.edtrading.data.Coord;
import borg.edtrading.util.MiscUtil;
import com.google.gson.annotations.SerializedName;
import org.apache.commons.csv.CSVRecord;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;

/**
 * StarSystem
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class StarSystem implements Serializable {

    private static final long serialVersionUID = -2199129316269236404L;

    static final Logger logger = LogManager.getLogger(StarSystem.class);

    private Long id = null;
    private String name = null;
    private Float x = null;
    private Float y = null;
    private Float z = null;
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
        this.setX(MiscUtil.getAsFloat(record.get("x")));
        this.setY(MiscUtil.getAsFloat(record.get("y")));
        this.setZ(MiscUtil.getAsFloat(record.get("z")));
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

    public float distanceTo(StarSystem other) {
        return this.getCoord().distanceTo(other.getCoord());
    }

    //    public float distanceManhattanTo(StarSystem other) {
    //        return this.getCoord().distanceManhattanTo(other.getCoord());
    //    }

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

    public Float getX() {
        return this.x;
    }

    public void setX(Float x) {
        this.x = x;
    }

    public Float getY() {
        return this.y;
    }

    public void setY(Float y) {
        this.y = y;
    }

    public Float getZ() {
        return this.z;
    }

    public void setZ(Float z) {
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

}
