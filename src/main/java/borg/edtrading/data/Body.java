package borg.edtrading.data;

import borg.edtrading.util.MiscUtil;
import com.google.gson.annotations.SerializedName;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.elasticsearch.common.xcontent.XContentFactory;

import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * Body
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Body {

    static final Logger logger = LogManager.getLogger(Body.class);

    public static final String ES_TYPE = "body";

    private Long id = null;
    private String name = null;
    @SerializedName("system_id")
    private Long starSystemId = null;
    private StarSystem starSystem = null;
    @SerializedName("group_name")
    private String groupName = null; // Star, Planet, ...
    @SerializedName("type_name")
    private String typeName = null; // Rocky body, High metal content world, ...
    private Long distance_to_arrival = null; // Ls, Missing decimal places
    private Double solar_masses = null;
    private Double earth_masses = null;
    private Double moon_masses = null;
    private Double radius = null; // KM
    private Double gravity = null; // G
    private Long surface_temperature = null; // K
    private List<SolidComposition> solid_composition = null;
    private Double orbital_period = null; // D
    private Double semi_major_axis = null; // AU
    private Double orbital_eccentricity = null;
    private Double orbital_inclination = null; // °
    private Double arg_of_periapsis = null; // °
    private Double rotational_period = null; // D
    private Double axis_tilt = null; // °

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
        Body other = (Body) obj;
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
        return String.format("#%d %s", this.getId(), this.getName());
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

    public Long getStarSystemId() {
        return this.starSystemId;
    }

    public void setStarSystemId(Long starSystemId) {
        this.starSystemId = starSystemId;
    }

    public StarSystem getStarSystem() {
        return this.starSystem;
    }

    public void setStarSystem(StarSystem starSystem) {
        this.starSystem = starSystem;
    }

    public String getGroupName() {
        return this.groupName;
    }

    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    public String getTypeName() {
        return this.typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    public Long getDistance_to_arrival() {
        return this.distance_to_arrival;
    }

    public void setDistance_to_arrival(Long distance_to_arrival) {
        this.distance_to_arrival = distance_to_arrival;
    }

    public Double getSolar_masses() {
        return this.solar_masses;
    }

    public void setSolar_masses(Double solar_masses) {
        this.solar_masses = solar_masses;
    }

    public Double getEarth_masses() {
        return this.earth_masses;
    }

    public void setEarth_masses(Double earth_masses) {
        this.earth_masses = earth_masses;
    }

    public Double getMoon_masses() {
        return this.moon_masses;
    }

    public void setMoon_masses(Double moon_masses) {
        this.moon_masses = moon_masses;
    }

    public Double getRadius() {
        return this.radius;
    }

    public void setRadius(Double radius) {
        this.radius = radius;
    }

    public Double getGravity() {
        return this.gravity;
    }

    public void setGravity(Double gravity) {
        this.gravity = gravity;
    }

    public Long getSurface_temperature() {
        return this.surface_temperature;
    }

    public void setSurface_temperature(Long surface_temperature) {
        this.surface_temperature = surface_temperature;
    }

    public List<SolidComposition> getSolid_composition() {
        return this.solid_composition;
    }

    public void setSolid_composition(List<SolidComposition> solid_composition) {
        this.solid_composition = solid_composition;
    }

    public Double getOrbital_period() {
        return this.orbital_period;
    }

    public void setOrbital_period(Double orbital_period) {
        this.orbital_period = orbital_period;
    }

    public Double getSemi_major_axis() {
        return this.semi_major_axis;
    }

    public void setSemi_major_axis(Double semi_major_axis) {
        this.semi_major_axis = semi_major_axis;
    }

    public Double getOrbital_eccentricity() {
        return this.orbital_eccentricity;
    }

    public void setOrbital_eccentricity(Double orbital_eccentricity) {
        this.orbital_eccentricity = orbital_eccentricity;
    }

    public Double getOrbital_inclination() {
        return this.orbital_inclination;
    }

    public void setOrbital_inclination(Double orbital_inclination) {
        this.orbital_inclination = orbital_inclination;
    }

    public Double getArg_of_periapsis() {
        return this.arg_of_periapsis;
    }

    public void setArg_of_periapsis(Double arg_of_periapsis) {
        this.arg_of_periapsis = arg_of_periapsis;
    }

    public Double getRotational_period() {
        return this.rotational_period;
    }

    public void setRotational_period(Double rotational_period) {
        this.rotational_period = rotational_period;
    }

    public Double getAxis_tilt() {
        return this.axis_tilt;
    }

    public void setAxis_tilt(Double axis_tilt) {
        this.axis_tilt = axis_tilt;
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

    public static Body fromElasticSearchSource(Map<String, Object> source) {
        Body object = new Body();

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

    public static class SolidComposition {

        private Long solid_component_id = null;
        private String solid_component_name = null;
        private Double share = null;

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
            SolidComposition other = (SolidComposition) obj;
            if (this.solid_component_id == null) {
                if (other.solid_component_id != null) {
                    return false;
                }
            } else if (!this.solid_component_id.equals(other.solid_component_id)) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.solid_component_id == null) ? 0 : this.solid_component_id.hashCode());
            return result;
        }

        @Override
        public String toString() {
            return String.format("%.f1f%% %s", this.getShare(), this.getSolid_component_name());
        }

        public Long getSolid_component_id() {
            return this.solid_component_id;
        }

        public void setSolid_component_id(Long solid_component_id) {
            this.solid_component_id = solid_component_id;
        }

        public String getSolid_component_name() {
            return this.solid_component_name;
        }

        public void setSolid_component_name(String solid_component_name) {
            this.solid_component_name = solid_component_name;
        }

        public Double getShare() {
            return this.share;
        }

        public void setShare(Double share) {
            this.share = share;
        }

    }

}
