package borg.edtrading.eddb.data;

import borg.edtrading.data.Coord;
import com.google.gson.annotations.SerializedName;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.util.Date;

/**
 * EddbMarketEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
@Document(indexName = "eddbmarketentry", type = "eddbmarketentry", shards = 4, replicas = 0)
public class EddbMarketEntry implements EddbEntity {

    private static final long serialVersionUID = -9190337609843319285L;

    private Coord coord = null;

    public Coord getCoord() {
        return this.coord;
    }

    public void setCoord(Coord coord) {
        this.coord = coord;
    }

    @Id
    @SerializedName("id")
    private Long id = null;
    @Field(type = FieldType.Date)
    @SerializedName("collected_at")
    private Date updatedAt = null;
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

    @Override
    public Date getUpdatedAt() {
        return this.updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
        this.updatedAt = updatedAt;
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

}
