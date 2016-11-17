package borg.edtrading.sidepanel;

import borg.edtrading.util.MiscUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * ShipModule
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipModule implements Serializable {

    private static final long serialVersionUID = -5120871377221693884L;

    static final Logger logger = LogManager.getLogger(ShipModule.class);

    private static final String[] CLASSES = { "E", "D", "C", "B", "A" };

    private String key = null;
    private String name = null;
    private Integer buyPrice = null;
    private Integer fuelCapacity = null;
    private Integer cargoCapacity = null;

    public ShipModule() {
        this(null);
    }

    public ShipModule(String key) {
        this(key, null);
    }

    public ShipModule(String key, String name) {
        this(key, name, null);
    }

    public ShipModule(String key, String name, Integer buyPrice) {
        this.setKey(key);
        this.setName(name);
        this.setBuyPrice(buyPrice);

        String sizeAndClass = keyToSizeAndClass(key);
        if (StringUtils.isNotEmpty(sizeAndClass) && StringUtils.isNotEmpty(name) && !name.contains(sizeAndClass)) {
            this.setName(sizeAndClass + " " + name);
        }

        if (StringUtils.isNotEmpty(sizeAndClass) && StringUtils.isNotEmpty(key)) {
            int size = Integer.valueOf(Character.toString(sizeAndClass.charAt(0)));
            int capacity = (int) Math.pow(2, size);
            if (key.contains("int_fueltank")) {
                this.setFuelCapacity(capacity);
            } else if (key.contains("int_cargorack")) {
                this.setCargoCapacity(capacity);
            }
        }
    }

    private static String keyToSizeAndClass(String key) {
        if (StringUtils.isNotEmpty(key)) {
            Pattern p = Pattern.compile(".+_size(\\d+)_class(\\d+)_.+");
            Matcher m = p.matcher(key);

            if (m.matches()) {
                int size = MiscUtil.getAsInt(m.group(1), -1);
                int clazz = MiscUtil.getAsInt(m.group(2), -1);

                if (size >= 0 && clazz >= 1 && clazz <= 5) {
                    return String.valueOf(size) + CLASSES[clazz - 1];
                }
            }
        }

        return "";
    }

    public String getKey() {
        return this.key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getBuyPrice() {
        return this.buyPrice;
    }

    public void setBuyPrice(Integer buyPrice) {
        this.buyPrice = buyPrice;
    }

    public Integer getFuelCapacity() {
        return this.fuelCapacity;
    }

    public void setFuelCapacity(Integer fuelCapacity) {
        this.fuelCapacity = fuelCapacity;
    }

    public Integer getCargoCapacity() {
        return this.cargoCapacity;
    }

    public void setCargoCapacity(Integer cargoCapacity) {
        this.cargoCapacity = cargoCapacity;
    }

}
