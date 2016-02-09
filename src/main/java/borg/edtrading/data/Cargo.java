package borg.edtrading.data;

import java.io.IOException;
import java.util.Map;

import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.elasticsearch.common.xcontent.XContentFactory;

/**
 * Cargo
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Cargo {

    static final Logger logger = LogManager.getLogger(Cargo.class);

    public static final String ES_TYPE = "cargo";

    private String name = null;
    private Long galacticAverage = null;

    public Cargo(String name, Long galacticAverage) {
        this.setName(name);
        this.setGalacticAverage(galacticAverage);
    }

    public static XContentBuilder createElasticSearchMapping() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject();

            builder.field("dynamic", "strict");
            builder.startObject("properties")

            // @formatter:off
            .startObject("name").field("type", "string").field("analyzer", "lowercaseKeyword").endObject()
            .startObject("galacticAverage").field("type", "long").endObject()
            // @formatter:on

            .endObject(); // END properties

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch mapping", e);
        }
    }

    public static Cargo fromElasticSearchSource(Map<String, Object> source) {
        String name = MiscUtil.getAsString(source.get("name"));
        Long galacticAverage = MiscUtil.getAsLong(source.get("galacticAverage"));

        return new Cargo(name, galacticAverage);
    }

    public XContentBuilder toElasticSearchSource() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject()

            // @formatter:off
            .field("name", this.getName())
            .field("galacticAverage", this.getGalacticAverage());
            // @formatter:on

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch source for " + this, e);
        }
    }

    public String getElasticSearchId() {
        return this.getName().toLowerCase();
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getGalacticAverage() {
        return this.galacticAverage;
    }

    public void setGalacticAverage(Long galacticAverage) {
        this.galacticAverage = galacticAverage;
    }

}
