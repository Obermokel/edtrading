package borg.edtrading.data;

import java.io.IOException;
import java.util.Map;

import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.elasticsearch.common.xcontent.XContentFactory;

/**
 * StarSystem
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class StarSystem {

    static final Logger logger = LogManager.getLogger(StarSystem.class);

    public static final String ES_TYPE = "star-system";

    private String name = null;

    public StarSystem(String name) {
        this.setName(name);
    }

    public static XContentBuilder createElasticSearchMapping() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject();

            builder.field("dynamic", "strict");
            builder.startObject("properties")
            .startObject("name").field("type", "string").field("analyzer", "lowercaseKeyword").endObject()
            .endObject(); // END properties

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch mapping", e);
        }
    }

    public static StarSystem fromElasticSearchSource(Map<String, Object> source) {
        String name = MiscUtil.getAsString(source.get("name"));

        return new StarSystem(name);
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

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

}
