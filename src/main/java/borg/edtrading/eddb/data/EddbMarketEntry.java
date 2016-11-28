package borg.edtrading.eddb.data;

import com.google.gson.annotations.SerializedName;

import java.util.Date;

/**
 * EddbMarketEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbMarketEntry implements EddbEntity {

    private static final long serialVersionUID = -9190337609843319285L;

    @SerializedName("id")
    private Long id = null;
    @SerializedName("station_id")
    private Long stationId = null;
    @SerializedName("commodity_id")
    private Long commodityId = null;
    @SerializedName("supply")
    private Integer supply = null;
    @SerializedName("buy_price")
    private Integer buyPrice = null;
    @SerializedName("demand")
    private Integer demand = null;
    @SerializedName("sell_price")
    private Integer sellPrice = null;
    @SerializedName("collected_at")
    private Date collectedAt = null;

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (this.getClass() != obj.getClass()) {
            return false;
        }
        EddbMarketEntry other = (EddbMarketEntry) obj;
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
        return this.id.hashCode();
    }

    @Override
    public String toString() {
        return "#" + this.id;
    }

    @Override
    public Long getId() {
        return this.id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getStationId() {
        return this.stationId;
    }

    public void setStationId(Long stationId) {
        this.stationId = stationId;
    }

    public Long getCommodityId() {
        return this.commodityId;
    }

    public void setCommodityId(Long commodityId) {
        this.commodityId = commodityId;
    }

    public Integer getSupply() {
        return this.supply;
    }

    public void setSupply(Integer supply) {
        this.supply = supply;
    }

    public Integer getBuyPrice() {
        return this.buyPrice;
    }

    public void setBuyPrice(Integer buyPrice) {
        this.buyPrice = buyPrice;
    }

    public Integer getDemand() {
        return this.demand;
    }

    public void setDemand(Integer demand) {
        this.demand = demand;
    }

    public Integer getSellPrice() {
        return this.sellPrice;
    }

    public void setSellPrice(Integer sellPrice) {
        this.sellPrice = sellPrice;
    }

    public Date getCollectedAt() {
        return this.collectedAt;
    }

    public void setCollectedAt(Date collectedAt) {
        this.collectedAt = collectedAt;
    }

}