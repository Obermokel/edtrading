package borg.edtrading.data;

import java.io.IOException;
import java.util.Date;
import java.util.Map;

import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.elasticsearch.common.xcontent.XContentFactory;

/**
 * TradingData
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TradingData {

    static final Logger logger = LogManager.getLogger(TradingData.class);

    public static final String ES_TYPE = "trading-data";

    private Date timestamp = null;
    private Station station = null;
    private Cargo cargo = null;
    private Long priceToSell = null; // What I get if I sell
    private Long priceToBuy = null; // What I have to pay if I buy

    public TradingData(Date timestamp, Station station, Cargo cargo, Long priceToSell, Long priceToBuy) {
        this.setTimestamp(timestamp);
        this.setStation(station);
        this.setCargo(cargo);
        this.setPriceToSell(priceToSell);
        this.setPriceToBuy(priceToBuy);
    }

    public static XContentBuilder createElasticSearchMapping() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject();

            builder.field("dynamic", "strict");
            builder.startObject("properties")

            // @formatter:off
            .startObject("timestamp").field("type", "date").endObject()
            .startObject("station").startObject("properties")
                .startObject("name").field("type", "string").field("analyzer", "lowercaseKeyword").endObject()
                .startObject("starSystem").startObject("properties")
                    .startObject("name").field("type", "string").field("analyzer", "lowercaseKeyword").endObject()
                .endObject().endObject()
                .startObject("distanceFromStarInLs").field("type", "double").endObject()
            .endObject().endObject()
            .startObject("cargo").startObject("properties")
                .startObject("name").field("type", "string").field("analyzer", "lowercaseKeyword").endObject()
                .startObject("galacticAverage").field("type", "long").endObject()
            .endObject().endObject()
            .startObject("priceToSell").field("type", "long").endObject()
            .startObject("priceToBuy").field("type", "long").endObject()
            // @formatter:on

            .endObject(); // END properties

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch mapping", e);
        }
    }

    public static TradingData fromElasticSearchSource(Map<String, Object> source) {
        Date timestamp = MiscUtil.getAsDate(source.get("timestamp"));
        Station station = Station.fromElasticSearchSource((Map<String, Object>) source.get("station"));
        Cargo cargo = null;
        Long priceToSell = MiscUtil.getAsLong(source.get("priceToSell"));
        Long priceToBuy = MiscUtil.getAsLong(source.get("priceToBuy"));

        return new TradingData(timestamp, station, cargo, priceToSell, priceToBuy);
    }

    public XContentBuilder toElasticSearchSource() {
        try {
            XContentBuilder builder = XContentFactory.jsonBuilder().humanReadable(true).startObject()

            // @formatter:off
            .field("timestamp", this.getTimestamp())
            .startObject("station")
                .field("name", this.getStation().getName())
                .startObject("starSystem")
                    .field("name", this.getStation().getStarSystem().getName())
                .endObject()
                .field("distanceFromStarInLs", this.getStation().getDistanceFromStarInLs())
            .endObject()
            .startObject("cargo")
                .field("name", this.getCargo().getName())
                .field("galacticAverage", this.getCargo().getGalacticAverage())
            .endObject()
            .field("priceToSell", this.getPriceToSell())
            .field("priceToBuy", this.getPriceToBuy());
            // @formatter:on

            return builder.endObject();
        } catch (IOException e) {
            throw new RuntimeException("Failed to create ElasticSearch source for " + this, e);
        }
    }

    public String getElasticSearchId() {
        return (this.getStation().getStarSystem().getName() + "_" + this.getStation().getName() + "_" + this.getCargo().getName()).toLowerCase();
    }

    public Date getTimestamp() {
        return this.timestamp;
    }

    public void setTimestamp(Date timestamp) {
        this.timestamp = timestamp;
    }

    public Station getStation() {
        return this.station;
    }

    public void setStation(Station station) {
        this.station = station;
    }

    public Cargo getCargo() {
        return this.cargo;
    }

    public void setCargo(Cargo cargo) {
        this.cargo = cargo;
    }

    public Long getPriceToSell() {
        return this.priceToSell;
    }

    public void setPriceToSell(Long priceToSell) {
        this.priceToSell = priceToSell;
    }

    public Long getPriceToBuy() {
        return this.priceToBuy;
    }

    public void setPriceToBuy(Long priceToBuy) {
        this.priceToBuy = priceToBuy;
    }

}
