package borg.edtrading.eddb.data;

import com.google.gson.annotations.SerializedName;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.math.BigDecimal;
import java.util.Date;

/**
 * EddbStation
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
@Document(indexName = "eddb", type = "station", shards = 10, replicas = 0)
public class EddbStation implements EddbEntity {

    private static final long serialVersionUID = 7221235081735765915L;

    private transient EddbSystem system = null;
    private transient EddbBody body = null;
    private transient EddbFaction controllingMinorFaction = null;

    public EddbSystem getSystem() {
        return this.system;
    }

    public void setSystem(EddbSystem system) {
        this.system = system;
    }

    public EddbBody getBody() {
        return this.body;
    }

    public void setBody(EddbBody body) {
        this.body = body;
    }

    public EddbFaction getControllingMinorFaction() {
        return this.controllingMinorFaction;
    }

    public void setControllingMinorFaction(EddbFaction controllingMinorFaction) {
        this.controllingMinorFaction = controllingMinorFaction;
    }

    @Id
    @SerializedName("id")
    private Long id = null;
    @Field(type = FieldType.Date)
    @SerializedName("updated_at")
    private Date updatedAt = null;
    @SerializedName("name")
    private String name = null;
    @SerializedName("system_id")
    private Long systemId = null;
    @SerializedName("body_id")
    private Long bodyId = null;
    @SerializedName("controlling_minor_faction_id")
    private Long controllingMinorFactionId = null;
    @SerializedName("max_landing_pad_size")
    private String maxLandingPadSize = null;
    @Field(type = FieldType.Double)
    @SerializedName("distance_to_star")
    private BigDecimal distanceToStar = null;
    @SerializedName("is_planetary")
    private Boolean isPlanetary = null;
    //    @SerializedName("economies")
    //    private List<String> economies = null;
    @SerializedName("government_id")
    private Long governmentId = null;
    @SerializedName("government")
    private String government = null;
    @SerializedName("allegiance_id")
    private Long allegianceId = null;
    @SerializedName("allegiance")
    private String allegiance = null;
    @SerializedName("state_id")
    private Long stateId = null;
    @SerializedName("state")
    private String state = null;
    @SerializedName("type_id")
    private Long typeId = null;
    @SerializedName("type")
    private String type = null;
    @SerializedName("settlement_size_id")
    private Long settlementSizeId = null;
    @SerializedName("settlement_size")
    private String settlementSize = null;
    @SerializedName("settlement_security_id")
    private Long settlementSecurityId = null;
    @SerializedName("settlement_security")
    private String settlementSecurity = null;
    @SerializedName("has_blackmarket")
    private Boolean hasBlackmarket = null;
    @SerializedName("has_market")
    private Boolean hasMarket = null;
    @SerializedName("has_refuel")
    private Boolean hasRefuel = null;
    @SerializedName("has_repair")
    private Boolean hasRepair = null;
    @SerializedName("has_rearm")
    private Boolean hasRearm = null;
    @SerializedName("has_outfitting")
    private Boolean hasOutfitting = null;
    @SerializedName("has_shipyard")
    private Boolean hasShipyard = null;
    @SerializedName("has_docking")
    private Boolean hasDocking = null;
    @SerializedName("has_commodities")
    private Boolean hasCommodities = null;
    //    @SerializedName("import_commodities")
    //    private List<String> importCommodities = null;
    //    @SerializedName("export_commodities")
    //    private List<String> exportCommodities = null;
    //    @SerializedName("prohibited_commodities")
    //    private List<String> prohibitedCommodities = null;
    //    @SerializedName("selling_ships")
    //    private List<String> sellingShips = null;
    //    @SerializedName("selling_modules")
    //    private List<Long> sellingModules = null;
    @Field(type = FieldType.Date)
    @SerializedName("shipyard_updated_at")
    private Date shipyardUpdatedAt = null;
    @Field(type = FieldType.Date)
    @SerializedName("outfitting_updated_at")
    private Date outfittingUpdatedAt = null;
    @Field(type = FieldType.Date)
    @SerializedName("market_updated_at")
    private Date marketUpdatedAt = null;

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
        EddbStation other = (EddbStation) obj;
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
        return "#" + this.id + " " + this.name;
    }

    @Override
    public Long getId() {
        return this.id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Date getUpdatedAt() {
        return this.updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
        this.updatedAt = updatedAt;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getSystemId() {
        return this.systemId;
    }

    public void setSystemId(Long systemId) {
        this.systemId = systemId;
    }

    public Long getBodyId() {
        return this.bodyId;
    }

    public void setBodyId(Long bodyId) {
        this.bodyId = bodyId;
    }

    public Long getControllingMinorFactionId() {
        return this.controllingMinorFactionId;
    }

    public void setControllingMinorFactionId(Long controllingMinorFactionId) {
        this.controllingMinorFactionId = controllingMinorFactionId;
    }

    public String getMaxLandingPadSize() {
        return this.maxLandingPadSize;
    }

    public void setMaxLandingPadSize(String maxLandingPadSize) {
        this.maxLandingPadSize = maxLandingPadSize == null ? null : maxLandingPadSize.intern();
    }

    public BigDecimal getDistanceToStar() {
        return this.distanceToStar;
    }

    public void setDistanceToStar(BigDecimal distanceToStar) {
        this.distanceToStar = distanceToStar;
    }

    public Boolean getIsPlanetary() {
        return this.isPlanetary;
    }

    public void setIsPlanetary(Boolean isPlanetary) {
        this.isPlanetary = isPlanetary;
    }

    //    public List<String> getEconomies() {
    //        return this.economies;
    //    }
    //
    //    public void setEconomies(List<String> economies) {
    //        if (economies == null || economies.isEmpty()) {
    //            this.economies = null;
    //        } else {
    //            List<String> list = new ArrayList<>(economies.size());
    //            for (String s : economies) {
    //                list.add(s.intern());
    //            }
    //            this.economies = list;
    //        }
    //    }

    public Long getGovernmentId() {
        return this.governmentId;
    }

    public void setGovernmentId(Long governmentId) {
        this.governmentId = governmentId;
    }

    public String getGovernment() {
        return this.government;
    }

    public void setGovernment(String government) {
        this.government = government == null ? null : government.intern();
    }

    public Long getAllegianceId() {
        return this.allegianceId;
    }

    public void setAllegianceId(Long allegianceId) {
        this.allegianceId = allegianceId;
    }

    public String getAllegiance() {
        return this.allegiance;
    }

    public void setAllegiance(String allegiance) {
        this.allegiance = allegiance == null ? null : allegiance.intern();
    }

    public Long getStateId() {
        return this.stateId;
    }

    public void setStateId(Long stateId) {
        this.stateId = stateId;
    }

    public String getState() {
        return this.state;
    }

    public void setState(String state) {
        this.state = state == null ? null : state.intern();
    }

    public Long getTypeId() {
        return this.typeId;
    }

    public void setTypeId(Long typeId) {
        this.typeId = typeId;
    }

    public String getType() {
        return this.type;
    }

    public void setType(String type) {
        this.type = type == null ? null : type.intern();
    }

    public Long getSettlementSizeId() {
        return this.settlementSizeId;
    }

    public void setSettlementSizeId(Long settlementSizeId) {
        this.settlementSizeId = settlementSizeId;
    }

    public String getSettlementSize() {
        return this.settlementSize;
    }

    public void setSettlementSize(String settlementSize) {
        this.settlementSize = settlementSize == null ? null : settlementSize.intern();
    }

    public Long getSettlementSecurityId() {
        return this.settlementSecurityId;
    }

    public void setSettlementSecurityId(Long settlementSecurityId) {
        this.settlementSecurityId = settlementSecurityId;
    }

    public String getSettlementSecurity() {
        return this.settlementSecurity;
    }

    public void setSettlementSecurity(String settlementSecurity) {
        this.settlementSecurity = settlementSecurity == null ? null : settlementSecurity.intern();
    }

    public Boolean getHasBlackmarket() {
        return this.hasBlackmarket;
    }

    public void setHasBlackmarket(Boolean hasBlackmarket) {
        this.hasBlackmarket = hasBlackmarket;
    }

    public Boolean getHasMarket() {
        return this.hasMarket;
    }

    public void setHasMarket(Boolean hasMarket) {
        this.hasMarket = hasMarket;
    }

    public Boolean getHasRefuel() {
        return this.hasRefuel;
    }

    public void setHasRefuel(Boolean hasRefuel) {
        this.hasRefuel = hasRefuel;
    }

    public Boolean getHasRepair() {
        return this.hasRepair;
    }

    public void setHasRepair(Boolean hasRepair) {
        this.hasRepair = hasRepair;
    }

    public Boolean getHasRearm() {
        return this.hasRearm;
    }

    public void setHasRearm(Boolean hasRearm) {
        this.hasRearm = hasRearm;
    }

    public Boolean getHasOutfitting() {
        return this.hasOutfitting;
    }

    public void setHasOutfitting(Boolean hasOutfitting) {
        this.hasOutfitting = hasOutfitting;
    }

    public Boolean getHasShipyard() {
        return this.hasShipyard;
    }

    public void setHasShipyard(Boolean hasShipyard) {
        this.hasShipyard = hasShipyard;
    }

    public Boolean getHasDocking() {
        return this.hasDocking;
    }

    public void setHasDocking(Boolean hasDocking) {
        this.hasDocking = hasDocking;
    }

    public Boolean getHasCommodities() {
        return this.hasCommodities;
    }

    public void setHasCommodities(Boolean hasCommodities) {
        this.hasCommodities = hasCommodities;
    }

    //    public List<String> getImportCommodities() {
    //        return this.importCommodities;
    //    }
    //
    //    public void setImportCommodities(List<String> importCommodities) {
    //        this.importCommodities = importCommodities == null ? null : importCommodities.stream().map(s -> s.intern()).collect(Collectors.toList());
    //    }
    //
    //    public List<String> getExportCommodities() {
    //        return this.exportCommodities;
    //    }
    //
    //    public void setExportCommodities(List<String> exportCommodities) {
    //        this.exportCommodities = exportCommodities == null ? null : exportCommodities.stream().map(s -> s.intern()).collect(Collectors.toList());
    //    }
    //
    //    public List<String> getProhibitedCommodities() {
    //        return this.prohibitedCommodities;
    //    }
    //
    //    public void setProhibitedCommodities(List<String> prohibitedCommodities) {
    //        this.prohibitedCommodities = prohibitedCommodities == null ? null : prohibitedCommodities.stream().map(s -> s.intern()).collect(Collectors.toList());
    //    }

    //    public List<String> getSellingShips() {
    //        return this.sellingShips;
    //    }
    //
    //    public void setSellingShips(List<String> sellingShips) {
    //        if (sellingShips == null || sellingShips.isEmpty()) {
    //            this.sellingShips = null;
    //        } else {
    //            List<String> list = new ArrayList<>(sellingShips.size());
    //            for (String s : sellingShips) {
    //                list.add(s.intern());
    //            }
    //            this.sellingShips = list;
    //        }
    //    }
    //
    //    public List<Long> getSellingModules() {
    //        return this.sellingModules;
    //    }
    //
    //    public void setSellingModules(List<Long> sellingModules) {
    //        if (sellingModules == null || sellingModules.isEmpty()) {
    //            this.sellingModules = null;
    //        } else {
    //            this.sellingModules = new ArrayList<>(sellingModules);
    //        }
    //    }

    public Date getShipyardUpdatedAt() {
        return this.shipyardUpdatedAt;
    }

    public void setShipyardUpdatedAt(Date shipyardUpdatedAt) {
        this.shipyardUpdatedAt = shipyardUpdatedAt;
    }

    public Date getOutfittingUpdatedAt() {
        return this.outfittingUpdatedAt;
    }

    public void setOutfittingUpdatedAt(Date outfittingUpdatedAt) {
        this.outfittingUpdatedAt = outfittingUpdatedAt;
    }

    public Date getMarketUpdatedAt() {
        return this.marketUpdatedAt;
    }

    public void setMarketUpdatedAt(Date marketUpdatedAt) {
        this.marketUpdatedAt = marketUpdatedAt;
    }

}
