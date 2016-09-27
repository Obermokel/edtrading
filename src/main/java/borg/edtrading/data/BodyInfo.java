package borg.edtrading.data;

import org.apache.commons.lang3.StringUtils;

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

    RESERVES_DEPLETED("Depleted Reserves"),
    RESERVES_LOW("Low Reserves"),
    RESERVES_COMMON("Common Reserves"),
    RESERVES_MAJOR("Major Reserves"),
    RESERVES_PRISTINE("Pristine Reserves"),

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
    ATMOSPHERE_TYPE_AMMONIA("Ammonia"),
    ATMOSPHERE_TYPE_AMMONIA_AND_OXYGEN("Ammonia and oxygen"),
    ATMOSPHERE_TYPE_ARGON("Argon"),
    ATMOSPHERE_TYPE_ARGON_RICH("Argon-rich"),
    ATMOSPHERE_TYPE_CARBON_DIOXIDE("Carbon dioxide"),
    ATMOSPHERE_TYPE_CARBON_DIOXIDE_RICH("Carbon dioxide-rich"),
    ATMOSPHERE_TYPE_HELIUM("Helium"),
    ATMOSPHERE_TYPE_METHANE("Methane"),
    ATMOSPHERE_TYPE_METHANE_RICH("Methane-rich"),
    ATMOSPHERE_TYPE_NEON("Neon"),
    ATMOSPHERE_TYPE_NEON_RICH("Neon-rich"),
    ATMOSPHERE_TYPE_NITROGEN("Nitrogen"),
    ATMOSPHERE_TYPE_NITROGEN_RICH("Nitrogen-rich"),
    ATMOSPHERE_TYPE_SILICATE_VAPOUR("Silicate vapour"),
    ATMOSPHERE_TYPE_SUITABLE_FOR_WATER_BASED_LIFE("Suitable for water based life"),
    ATMOSPHERE_TYPE_SULPHUR_DIOXIDE("Sulphur dioxide"),
    ATMOSPHERE_TYPE_WATER("Water"),
    ATMOSPHERE_TYPE_WATER_RICH("Water-rich"),
    ATMOSPHERE_TYPE_OXYGEN("Oxygen"),
    ATMOSPHERE_TYPE_MATEALLIC_VAPOUR("Mateallic vapour"), // TODO typo?
    ATMOSPHERE_TYPE_AMMONIA_RICH("Ammonia-rich"),

    ATMOSPHERE_COMPONENT_AMMONIA("Ammonia"),
    ATMOSPHERE_COMPONENT_ARGON("Argon"),
    ATMOSPHERE_COMPONENT_CARBON_DIOXIDE("Carbon dioxide"),
    ATMOSPHERE_COMPONENT_HYDROGEN("Hydrogen"),
    ATMOSPHERE_COMPONENT_HELIUM("Helium"),
    ATMOSPHERE_COMPONENT_IRON("Iron"),
    ATMOSPHERE_COMPONENT_NEON("Neon"),
    ATMOSPHERE_COMPONENT_METHANE("Methane"),
    ATMOSPHERE_COMPONENT_NITROGEN("Nitrogen"),
    ATMOSPHERE_COMPONENT_OXYGEN("Oxygen"),
    ATMOSPHERE_COMPONENT_SILICATES("Silicates"),
    ATMOSPHERE_COMPONENT_SULPHUR_DIOXIDE("Sulphur dioxide"),
    ATMOSPHERE_COMPONENT_WATER("Water");
    //@formatter:on

    private final String name;

    private BodyInfo(String name) {
        this.name = name;
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
