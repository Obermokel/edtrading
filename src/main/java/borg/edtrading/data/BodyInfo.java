package borg.edtrading.data;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Body
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum BodyInfo {

    //@formatter:off
    GROUP_STAR("Star"),
    GROUP_PLANET("Planet"),
    GROUP_BELT("Belt"),
    GROUP_RINGS("Rings"), // Not a real body type. Just an indicator for a scrolled-down screenshot of rings.

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

    // O, B, Neutron, BH
    STAR_TYPE_CLASS_A("Class A stars are hot white or bluish white main"), // 2016-10-02 06-57-39 Sirius.png
    STAR_TYPE_CLASS_F("Class F stars are white main sequence stars. They"), // 2016-10-06 22-55-53 Delta Equulei.png
    STAR_TYPE_CLASS_G("Class G stars are white-yellow main sequence"), // 2016-09-28 08-21-26 Eta Coronae Borealis.png
    STAR_TYPE_CLASS_K("Class K stars are yellow-orange main sequence"), // 2016-10-02 09-52-08 Skogulumari.png
    STAR_TYPE_CLASS_L("Class L dwarfs are dwarf stars that are cooler than"), // 2016-10-10 04-03-43 HIP 9774.png
    STAR_TYPE_CLASS_M("Class M stars are red stars that form the bulk of"), // 2016-10-03 11-48-27 Yimanbin.png
    STAR_TYPE_CLASS_T("Class T dwarfs are brown dwarfs with a surface"), // 2016-10-23 12-31-14 Agastariya (1).png
    STAR_TYPE_CLASS_Y("Class Y dwarfs are the coolest of the brown dwarfs"), // 2016-10-02 07-09-36 Xi Ursae Majoris.png
    STAR_TYPE_T_TAURI("T Tauri type stars are very young stars"), // 2016-10-03 13-35-08 Meliontit.png
    STAR_TYPE_WHITE_DWARF("White Dwarf stars are stellar remnants. Nuclear"), // 2016-10-02 06-57-48 Sirius.png

    TERRAFORMING_NOT_TERRAFORMABLE("Not terraformable"),
    TERRAFORMING_CANDIDATE_FOR_TERRAFORMING("Candidate for terraforming"),
    TERRAFORMING_BEING_TERRAFORMED("Being terraformed"),
    TERRAFORMING_TERRAFORMING_COMPLETED("Terraforming completed"),

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
    ATMOSPHERE_TYPE_SUITABLE_FOR_WATER_BASED_LIFE("SUITABLE FOR WATER-BASED LIFE"),
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
    ATMOSPHERE_COMPONENT_WATER("WATER"),

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
    RESERVES_PRISTINE("Pristine reserves");
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
        Collections.sort(result, new Comparator<BodyInfo>() {
            @Override
            public int compare(BodyInfo bi1, BodyInfo bi2) {
                return -1 * new Integer(bi1.getName().length()).compareTo(new Integer(bi2.getName().length()));
            }
        });
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
