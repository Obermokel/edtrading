package borg.edtrading.util;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;

/**
 * InventoryUtil
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class InventoryUtil {

    static final Logger logger = LogManager.getLogger(InventoryUtil.class);

    // Full HD
    //    private static final int INVENTORY_X = 600;
    //    private static final int INVENTORY_Y = 220;
    //    private static final int INVENTORY_WIDTH = 1120;
    //    private static final int INVENTORY_HEIGHT = 700;

    // 4k
    private static final int INVENTORY_X = 1800;
    private static final int INVENTORY_Y = 750;
    private static final int INVENTORY_WIDTH = 1400;
    private static final int INVENTORY_HEIGHT = 850;

    public static BufferedImage fullHdToInventory(BufferedImage fullHdImage) {
        return fullHdImage.getSubimage(INVENTORY_X / 2, INVENTORY_Y / 2, INVENTORY_WIDTH / 2, INVENTORY_HEIGHT / 2);
    }

    public static BufferedImage fourKToInventory(BufferedImage fourKImage) {
        return fourKImage.getSubimage(INVENTORY_X, INVENTORY_Y, INVENTORY_WIDTH, INVENTORY_HEIGHT);
    }

    public static BufferedImage inventoryToHeadline(BufferedImage inventoryImage) {
        return inventoryImage.getSubimage(0, 0, INVENTORY_WIDTH, 120);
    }

}
