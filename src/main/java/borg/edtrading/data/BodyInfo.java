package borg.edtrading.data;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Body
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum BodyInfo {

    //@formatter:off
    TYPE_AMMONIA_WORLD("Ammonia world"),
    TYPE_CLASS_I_GAS_GIANT("Class I gas giant"),
    TYPE_CLASS_II_GAS_GIANT("Class II gas giant"),
    TYPE_CLASS_III_GAS_GIANT("Class III gas giant"),
    TYPE_CLASS_IV_GAS_GIANT("Class IV gas giant"),
    TYPE_CLASS_V_GAS_GIANT("Class V gas giant"),
    TYPE_EARTH_LIKE_WORLD("Earth-like world"),
    TYPE_GAS_GIANT_WITH_AMMONIA_BASED_LIFE("Gas giant with ammonia-based life"),
    TYPE_GAS_GIANT_WITH_WATER_BASED_LIFE("Gas giant with water-based life"),
    TYPE_HELIUM_RICH_GAS_GIANT("Helium-rich gas giant"),
    TYPE_HIGH_METAL_CONTENT_WORLD("High metal content world"),
    TYPE_ICY_BODY("Icy body"),
    TYPE_METAL_RICH_BODY("Metal-rich body"),
    TYPE_ROCKY_BODY("Rocky body"),
    TYPE_ROCKY_ICE_WORLD("Rocky ice world"),
    TYPE_WATER_GIANT("Water giant"),
    TYPE_WATER_WORLD("Water world"),

    GROUP_STAR("Star"),
    GROUP_PLANET("Planet"),
    GROUP_BELT("Belt"),

    TERRAFORMING_NOT_TERRAFORMABLE("Not terraformable"),
    TERRAFORMING_CANDIDATE_FOR_TERRAFORMING("Candidate for terraforming"),
    TERRAFORMING_BEING_TERRAFORMED("Being terraformed"),
    TERRAFORMING_TERRAFORMING_COMPLETED("Terraforming completed"),

    COMPOSITION_ICE("ICE"),
    COMPOSITION_METAL("METAL"),
    COMPOSITION_ROCK("ROCK"),

    RING_TYPE_METALLIC("METALLIC"),
    RING_TYPE_METAL_RICH("METAL RICH"),
    RING_TYPE_ROCKY("ROCKY"),
    RING_TYPE_ICY("ICY"),

    RESERVES_DEPLETED("Depleted reserves"),
    RESERVES_LOW("Low reserves"),
    RESERVES_COMMON("Common reserves"),
    RESERVES_MAJOR("Major reserves"),
    RESERVES_PRISTINE("Pristine reserves"),

    VOLCANISM_NO_VOLCANISM("NO VOLCANISM"),
    VOLCANISM_AMMONIA_MAGMA("AMMONIA MAGMA"),
    VOLCANISM_CARBON_DIOXIDE_GEYSERS("CARBON DIOXIDE GEYSERS"),
    VOLCANISM_IRON_MAGMA("IRON MAGMA"),
    VOLCANISM_METHANE_MAGMA("METHANE MAGMA"),
    VOLCANISM_NITROGEN_MAGMA("NITROGEN MAGMA"),
    VOLCANISM_SILICATE_MAGMA("SILICATE MAGMA"),
    VOLCANISM_SILICATE_VAPOUR_GEYSERS("SILICATE VAPOUR GEYSERS"),
    VOLCANISM_WATER_GEYSERS("WATER GEYSERS"),
    VOLCANISM_WATER_MAGMA("WATER MAGMA"),

    ATMOSPHERE_TYPE_NO_ATMOSPHERE("NO ATMOSPHERE"),
    ATMOSPHERE_TYPE_AMMONIA("AMMONIA"),
    ATMOSPHERE_TYPE_AMMONIA_AND_OXYGEN("AMMONIA AND OXYGEN"),
    ATMOSPHERE_TYPE_ARGON("ARGON"),
    ATMOSPHERE_TYPE_ARGON_RICH("ARGON-RICH"),
    ATMOSPHERE_TYPE_CARBON_DIOXIDE("CARBON DIOXIDE"),
    ATMOSPHERE_TYPE_CARBON_DIOXIDE_RICH("CARBON DIOXIDE-RICH"),
    ATMOSPHERE_TYPE_HELIUM("HELIUM"),
    ATMOSPHERE_TYPE_METHANE("METHANE"),
    ATMOSPHERE_TYPE_METHANE_RICH("METHANE-RICH"),
    ATMOSPHERE_TYPE_NEON("NEON"),
    ATMOSPHERE_TYPE_NEON_RICH("NEON-RICH"),
    ATMOSPHERE_TYPE_NITROGEN("NITROGEN"),
    ATMOSPHERE_TYPE_NITROGEN_RICH("NITROGEN-RICH"),
    ATMOSPHERE_TYPE_SILICATE_VAPOUR("SILICATE VAPOUR"),
    ATMOSPHERE_TYPE_SUITABLE_FOR_WATER_BASED_LIFE("SUITABLE FOR WATER BASED LIFE"),
    ATMOSPHERE_TYPE_SULPHUR_DIOXIDE("SULPHUR DIOXIDE"),
    ATMOSPHERE_TYPE_WATER("WATER"),
    ATMOSPHERE_TYPE_WATER_RICH("WATER-RICH"),
    ATMOSPHERE_TYPE_OXYGEN("OXYGEN"),
    ATMOSPHERE_TYPE_MATEALLIC_VAPOUR("MATEALLIC VAPOUR"), // TODO typo?
    ATMOSPHERE_TYPE_AMMONIA_RICH("AMMONIA-RICH"),

    ATMOSPHERE_COMPONENT_AMMONIA("AMMONIA"),
    ATMOSPHERE_COMPONENT_ARGON("ARGON"),
    ATMOSPHERE_COMPONENT_CARBON_DIOXIDE("CARBON DIOXIDE"),
    ATMOSPHERE_COMPONENT_HYDROGEN("HYDROGEN"),
    ATMOSPHERE_COMPONENT_HELIUM("HELIUM"),
    ATMOSPHERE_COMPONENT_IRON("IRON"),
    ATMOSPHERE_COMPONENT_NEON("NEON"),
    ATMOSPHERE_COMPONENT_METHANE("METHANE"),
    ATMOSPHERE_COMPONENT_NITROGEN("NITROGEN"),
    ATMOSPHERE_COMPONENT_OXYGEN("OXYGEN"),
    ATMOSPHERE_COMPONENT_SILICATES("SILICATES"),
    ATMOSPHERE_COMPONENT_SULPHUR_DIOXIDE("SULPHUR DIOXIDE"),
    ATMOSPHERE_COMPONENT_WATER("WATER");
    //@formatter:on

    private final String name;

    private BodyInfo(String name) {
        this.name = name;
    }

    public static List<BodyInfo> byPrefix(String enumPrefix) {
        List<BodyInfo> result = new ArrayList<>();
        for (BodyInfo bodyInfo : BodyInfo.values()) {
            if (bodyInfo.name().startsWith(enumPrefix)) {
                result.add(bodyInfo);
            }
        }
        return result;
    }

    public static BodyInfo findBestMatching(String name, String enumPrefix) {
        BodyInfo bestBodyInfo = null;
        float bestBodyInfoError = Float.MAX_VALUE;
        for (BodyInfo bodyInfo : BodyInfo.values()) {
            if (bodyInfo.name().startsWith(enumPrefix)) {
                float dist = StringUtils.getLevenshteinDistance(name.toLowerCase().replaceAll("\\s", ""), bodyInfo.getName().toLowerCase().replaceAll("\\s", ""));
                float len = bodyInfo.getName().replaceAll("\\s", "").length();
                float err = dist / len;
                if (err <= 0.25f) {
                    if (err < bestBodyInfoError) {
                        bestBodyInfo = bodyInfo;
                        bestBodyInfoError = err;
                    }
                }
            }
        }
        return bestBodyInfo;
    }

    public String getName() {
        return this.name;
    }

}
