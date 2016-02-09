package borg.edtrading.data;

import java.io.IOException;
import java.util.Map;

import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.elasticsearch.common.xcontent.XContentFactory;

/**
 * Station
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Station {

    static final Logger logger = LogManager.getLogger(Station.class);

    public static final String ES_TYPE = "station";

    private String name = null;
    private StarSystem starSystem = null;
    private double distanceFromStarInLs = 0.0;

    public Station(String name, StarSystem starSystem, double distanceFromStarInLs) {
        this.setName(name);
        this.setStarSystem(starSystem);
        this.setDistanceFromStarInLs(distanceFromStarInLs);
    }

    public static XContentBuilder createElasticSearchMapping() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject();

            builder.field("dynamic", "strict");
            builder.startObject("properties")
            .startObject("name").field("type", "string").field("analyzer", "lowercaseKeyword").endObject()
            .startObject("starSystem").startObject("properties")
            .startObject("name").field("type", "string").field("analyzer", "lowercaseKeyword").endObject()
            .endObject().endObject() // END starSystem.properties
            .startObject("distanceFromStarInLs").field("type", "double").endObject()
            .endObject(); // END properties

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch mapping", e);
        }
    }

    public static Station fromElasticSearchSource(Map<String, Object> source) {
        String name = MiscUtil.getAsString(source.get("name"));
        StarSystem starSystem = StarSystem.fromElasticSearchSource((Map<String, Object>) source.get("starSystem"));
        double distanceFromStarInLs = MiscUtil.getAsDouble(source.get("distanceFromStarInLs"));

        return new Station(name, starSystem, distanceFromStarInLs);
    }

    public XContentBuilder toElasticSearchSource() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject();

            builder.field("name", this.getName());
            builder.startObject("starSystem").field("name", this.getStarSystem().getName()).endObject();
            builder.field("distanceFromStarInLs", this.getDistanceFromStarInLs());

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch source for " + this, e);
        }
    }

    public String getElasticSearchId() {
        return (this.getStarSystem().getName() + "_" + this.getName()).toLowerCase();
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public StarSystem getStarSystem() {
        return this.starSystem;
    }

    public void setStarSystem(StarSystem starSystem) {
        this.starSystem = starSystem;
    }

    public double getDistanceFromStarInLs() {
        return this.distanceFromStarInLs;
    }

    public void setDistanceFromStarInLs(double distanceFromStarInLs) {
        this.distanceFromStarInLs = distanceFromStarInLs;
    }

}
