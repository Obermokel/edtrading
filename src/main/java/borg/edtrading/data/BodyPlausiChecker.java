package borg.edtrading.data;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * BodyPlausiChecker
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyPlausiChecker {

    static final Logger logger = LogManager.getLogger(BodyPlausiChecker.class);

    public static final BigDecimal GRAVITATIONAL_CONSTANT = new BigDecimal("0.0000000000667408"); // m³ / (kg * s²)
    public static final BigDecimal EARTH_RADIUS_KM = new BigDecimal("6378"); // km
    public static final BigDecimal EARTH_MASS_KG = new BigDecimal("5974000000000000000000000"); // kg
    public static final BigDecimal EARTH_DENSITY_G_CM3 = new BigDecimal("5.515"); // g/cm³
    public static final BigDecimal EARTH_GRAVITY_M_S2 = new BigDecimal("9.81"); // m/s²

    private static final BigDecimal FACTOR_KM_TO_M = new BigDecimal(1000); // km -> m
    private static final BigDecimal FACTOR_KG_TO_G = new BigDecimal(1000); // kg -> g
    private static final BigDecimal FACTOR_KM3_TO_CM3 = new BigDecimal(100000).pow(3); // km³ -> cm³

    public static List<String> checkPlanet(BigDecimal radiusKm, BigDecimal earthMasses, BigDecimal gravityG) {
        List<String> messages = new ArrayList<>();

        if (radiusKm != null && earthMasses != null && gravityG != null) {
            BigDecimal massKg = earthMasses.multiply(EARTH_MASS_KG);
            BigDecimal gravityM_s2 = gravityG.multiply(EARTH_GRAVITY_M_S2);
            BigDecimal volumeKm3 = BigDecimal.valueOf(4.0 / 3.0).multiply(BigDecimal.valueOf(Math.PI)).multiply(radiusKm.pow(3)); // V = 4/3 * PI * r^3
            BigDecimal densityG_cm3 = (massKg.multiply(FACTOR_KG_TO_G)).divide(volumeKm3.multiply(FACTOR_KM3_TO_CM3), 3, BigDecimal.ROUND_HALF_UP); // d = m / V
            BigDecimal expectedGravityM_s2 = (GRAVITATIONAL_CONSTANT.multiply(massKg)).divide(radiusKm.multiply(FACTOR_KM_TO_M).pow(2), 2, BigDecimal.ROUND_HALF_UP); // g = (G * m) / r²
            BigDecimal expectedGravityG = expectedGravityM_s2.divide(EARTH_GRAVITY_M_S2, 2, BigDecimal.ROUND_HALF_UP);

            // Checks...
            double earthDensity = densityG_cm3.doubleValue() / EARTH_DENSITY_G_CM3.doubleValue();
            if (earthDensity < 0.3) {
                messages.add("Very low density: " + densityG_cm3 + " g/cm³, earth has " + EARTH_DENSITY_G_CM3 + " g/cm³");
            } else if (earthDensity > 1.8) {
                messages.add("Very high density: " + densityG_cm3 + " g/cm³, earth has " + EARTH_DENSITY_G_CM3 + " g/cm³");
            }
            double gravityOffsetFactor = gravityM_s2.doubleValue() / expectedGravityM_s2.doubleValue();
            if (gravityOffsetFactor < 0.9 && expectedGravityG.doubleValue() > 0.07) {
                messages.add("Very low gravity: " + gravityM_s2 + " m/s² (" + gravityG + "G), expected " + expectedGravityM_s2 + " m/s² (" + expectedGravityG + "G)");
            } else if (gravityOffsetFactor > 1.1 && expectedGravityG.doubleValue() > 0.07) {
                messages.add("Very high gravity: " + gravityM_s2 + " m/s² (" + gravityG + "G), expected " + expectedGravityM_s2 + " m/s² (" + expectedGravityG + "G)");
            }
        }

        return messages;
    }

}
