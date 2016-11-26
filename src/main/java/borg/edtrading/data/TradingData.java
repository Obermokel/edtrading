package borg.edtrading.data;

import borg.edtrading.eddb.data.Station;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Date;

/**
 * TradingData
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TradingData {

    static final Logger logger = LogManager.getLogger(TradingData.class);

    private Date timestamp = null;
    private Station station = null;
    private Commodity commodity = null;
    private Long priceToSell = null; // What I get if I sell
    private Long priceToBuy = null; // What I have to pay if I buy

    public TradingData(Date timestamp, Station station, Commodity commodity, Long priceToSell, Long priceToBuy) {
        this.setTimestamp(timestamp);
        this.setStation(station);
        this.setCommodity(commodity);
        this.setPriceToSell(priceToSell);
        this.setPriceToBuy(priceToBuy);
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

    public Commodity getCommodity() {
        return this.commodity;
    }

    public void setCommodity(Commodity commodity) {
        this.commodity = commodity;
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
