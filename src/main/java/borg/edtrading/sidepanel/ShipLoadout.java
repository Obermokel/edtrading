package borg.edtrading.sidepanel;

import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * ShipLoadout
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipLoadout implements Serializable {

    private static final long serialVersionUID = -8451008803793064647L;

    static final Logger logger = LogManager.getLogger(ShipLoadout.class);

    private Integer shipID = null;
    private String shipType = null;
    private String shipName = null;
    private Integer buyPrice = null;
    private Float maxFuelPerJump = null;
    private Float optTankJumpRange = null;
    private Float fullTankJumpRange = null;
    private SortedMap<String, ShipModule> modulesBySlot = new TreeMap<>();

    public ShipLoadout() {
        this(null, null);
    }

    public ShipLoadout(Integer shipID, String shipType) {
        this(shipID, shipType, null);
    }

    public ShipLoadout(Integer shipID, String shipType, Integer buyPrice) {
        this.setShipID(shipID);
        this.setShipType(shipType);
        this.setBuyPrice(buyPrice);
    }

    public int getShipValue() {
        int shipValue = MiscUtil.getAsInt(this.getBuyPrice(), 0);
        for (ShipModule module : this.getModulesBySlot().values()) {
            shipValue += MiscUtil.getAsInt(module.getBuyPrice(), 0);
        }
        return shipValue;
    }

    public int getShipInsurance() {
        return this.getShipInsurance(0.95f);
    }

    public int getShipInsurance(float insuranceLevel) {
        return Math.round((1.0f - insuranceLevel) * this.getShipValue());
    }

    public int getFuelCapacity() {
        int capacity = 0;
        for (ShipModule module : this.getModulesBySlot().values()) {
            capacity += MiscUtil.getAsInt(module.getFuelCapacity(), 0);
        }
        return capacity;
    }

    public int getCargoCapacity() {
        int capacity = 0;
        for (ShipModule module : this.getModulesBySlot().values()) {
            capacity += MiscUtil.getAsInt(module.getCargoCapacity(), 0);
        }
        return capacity;
    }

    public Integer getShipID() {
        return this.shipID;
    }

    public void setShipID(Integer shipID) {
        this.shipID = shipID;
    }

    public String getShipType() {
        return this.shipType;
    }

    public void setShipType(String shipType) {
        this.shipType = shipType;
    }

    public String getShipName() {
        return this.shipName;
    }

    public void setShipName(String shipName) {
        this.shipName = shipName;
    }

    public Integer getBuyPrice() {
        return this.buyPrice;
    }

    public void setBuyPrice(Integer buyPrice) {
        this.buyPrice = buyPrice;
    }

    public Float getMaxFuelPerJump() {
        return this.maxFuelPerJump;
    }

    public void setMaxFuelPerJump(Float maxFuelPerJump) {
        this.maxFuelPerJump = maxFuelPerJump;
    }

    public Float getOptTankJumpRange() {
        return this.optTankJumpRange;
    }

    public void setOptTankJumpRange(Float optTankJumpRange) {
        this.optTankJumpRange = optTankJumpRange;
    }

    public Float getFullTankJumpRange() {
        return this.fullTankJumpRange;
    }

    public void setFullTankJumpRange(Float fullTankJumpRange) {
        this.fullTankJumpRange = fullTankJumpRange;
    }

    public SortedMap<String, ShipModule> getModulesBySlot() {
        return this.modulesBySlot;
    }

    public void setModulesBySlot(SortedMap<String, ShipModule> modulesBySlot) {
        this.modulesBySlot = modulesBySlot;
    }

}
