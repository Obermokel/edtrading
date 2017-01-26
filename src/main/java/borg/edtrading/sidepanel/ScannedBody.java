package borg.edtrading.sidepanel;

import borg.edtrading.journal.entries.exploration.ScanEntry;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * ScannedBody
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScannedBody implements Serializable {

    private static final long serialVersionUID = 6942164845646742037L;

    static final Logger logger = LogManager.getLogger(ScannedBody.class);

    public static final int AVG_JUMP_PAYOUT = 4883;
    public static final Map<String, Integer> PAYOUTS = new HashMap<>();
    static {
      //@formatter:off
      PAYOUTS.put(                                "Class O", 3968);
      PAYOUTS.put(                                "Class B", 3060);//okay
      PAYOUTS.put(                                "Class A", 2950);//okay
      PAYOUTS.put(                                "Class F", 2938);//okay
      PAYOUTS.put(                                "Class G", 2918);//okay
      PAYOUTS.put(                                "Class K", 2910);//okay
      PAYOUTS.put(                       "Class K_RedGiant", 2900);
      PAYOUTS.put(                    "Class K_OrangeGiant", 2900);
      PAYOUTS.put(                                "Class M", 2894);//okay
      PAYOUTS.put(                       "Class M_RedGiant", 2900);
      PAYOUTS.put(                               "Class WC", 2911);
      PAYOUTS.put(                                "Class C", 2902);
      PAYOUTS.put(                                "Class L", 2886);//okay
      PAYOUTS.put(                                "Class Y", 2880);//okay
      PAYOUTS.put(                                "Class T", 2882);//okay
      PAYOUTS.put(                              "Class TTS", 2904);
      PAYOUTS.put(                             "Class AeBe", 3112);
      PAYOUTS.put(                               "Class MS", 2924);
      PAYOUTS.put(                               "Class CN", 2922);
      PAYOUTS.put(                                "Class S", 2928);

      PAYOUTS.put(                                "Class N", 43446);
      PAYOUTS.put(                                "Class H", 44948);
      PAYOUTS.put(            "Class SupermassiveBlackHole", 628318);
      PAYOUTS.put(                               "Class DA", 27146);
      PAYOUTS.put(                               "Class DB", 27156);
      PAYOUTS.put(                              "Class DAB", 27106);
      PAYOUTS.put(                              "Class DAV", 27148);
      PAYOUTS.put(                               "Class DC", 27017);

      PAYOUTS.put(                          "Ammonia world", 40734);
      PAYOUTS.put(                         "Earthlike body", 72148);
      PAYOUTS.put(                            "Water world", 30630);//okay
      PAYOUTS.put(                            "Water giant", 1519);
      PAYOUTS.put("High metal content body (Terraformable)", 45568);
      PAYOUTS.put(            "Water world (Terraformable)", 63110);//okay

      PAYOUTS.put(                  "Helium rich gas giant", (1467+2119+2733)/3);
      PAYOUTS.put(      "Gas giant with ammonia based life", 1850);//okay
      PAYOUTS.put(        "Gas giant with water based life", 2072);//okay
      PAYOUTS.put(             "Sudarsky class I gas giant", 3572);//okay
      PAYOUTS.put(            "Sudarsky class II gas giant", 13608);//okay
      PAYOUTS.put(           "Sudarsky class III gas giant", 2348);
      PAYOUTS.put(            "Sudarsky class IV gas giant", 2804);
      PAYOUTS.put(             "Sudarsky class V gas giant", 1865);

      PAYOUTS.put(                "High metal content body", 6822);//okay
      PAYOUTS.put(                               "Icy body", 1128);
      PAYOUTS.put(                        "Metal rich body", 12556);
      PAYOUTS.put(                             "Rocky body", 836);
      PAYOUTS.put(             "Rocky body (Terraformable)", 36782);
      PAYOUTS.put(                         "Rocky ice body", 1098);
      //@formatter:on
    }

    private ScanEntry scanEntry = null;
    private String bodyType = null;
    private String payoutKey = null;
    private boolean firstDiscovered = false;
    private int remainingBasePayout = 0;
    private int remainingBonusPayout = 0;

    public ScannedBody() {
        // Default
    }

    public ScannedBody(ScanEntry e) {
        this.scanEntry = e;
        this.bodyType = toBodyType(e);
        this.payoutKey = toPayoutKey(e);
        this.firstDiscovered = false;
        this.remainingBasePayout = PAYOUTS.getOrDefault(this.payoutKey, 999999999);
        this.remainingBonusPayout = 0;
    }

    private static String toBodyType(ScanEntry e) {
        if (StringUtils.isNotEmpty(e.getPlanetClass())) {
            String planetClass = e.getPlanetClass();
            if (StringUtils.isNotEmpty(e.getTerraformState())) {
                planetClass += (" (" + e.getTerraformState() + ")");
            }
            return planetClass;
        } else if (StringUtils.isNotEmpty(e.getStarType())) {
            if ("H".equals(e.getStarType())) {
                return "Black hole";
            } else if ("N".equals(e.getStarType())) {
                return "Neutron star";
            } else if ("DA".equals(e.getStarType()) || "DB".equals(e.getStarType()) || "DC".equals(e.getStarType())) {
                return "White dwarf";
            } else if ("TTS".equals(e.getStarType())) {
                return "T-Tauri star";
            } else if ("C".equals(e.getStarType())) {
                return "Carbon star";
            } else if ("WC".equals(e.getStarType())) {
                return "Wolf-Rayet star";
            } else if ("L".equals(e.getStarType()) || "T".equals(e.getStarType()) || "Y".equals(e.getStarType())) {
                return "Class " + e.getStarType() + " dwarf";
            } else if ("M_RedGiant".equals(e.getStarType()) || "K_RedGiant".equals(e.getStarType())) {
                return "Class " + e.getStarType().replace("_RedGiant", "") + " red giant";
            } else if ("M_OrangeGiant".equals(e.getStarType()) || "K_OrangeGiant".equals(e.getStarType())) {
                return "Class " + e.getStarType().replace("_OrangeGiant", "") + " orange giant";
            } else {
                return "Class " + e.getStarType() + " star";
            }
        } else {
            return e.getBodyName();
        }
    }

    private static String toPayoutKey(ScanEntry e) {
        if (StringUtils.isNotEmpty(e.getPlanetClass())) {
            String planetClass = e.getPlanetClass();
            if (StringUtils.isNotEmpty(e.getTerraformState())) {
                planetClass += (" (" + e.getTerraformState() + ")");
            }
            return planetClass;
        } else if (StringUtils.isNotEmpty(e.getStarType())) {
            return "Class " + e.getStarType();
        } else {
            return e.getBodyName();
        }
    }

    public String getStarClass() {
        return this.scanEntry.getStarType();
    }

    public String getBodyType() {
        return this.bodyType;
    }

    public String getPayoutKey() {
        return this.payoutKey;
    }

    public boolean isFirstDiscovered() {
        return this.firstDiscovered;
    }

    public int getRemainingBasePayout() {
        return this.remainingBasePayout;
    }

    public int getRemainingBonusPayout() {
        return this.remainingBonusPayout;
    }

    public void setToFirstDiscovered() {
        this.firstDiscovered = true;
        this.remainingBonusPayout = this.remainingBasePayout / 2;
    }

    public void setToNotFirstDiscovered() {
        this.firstDiscovered = false;
        this.remainingBonusPayout = 0;
    }

    public void setToPayedOut() {
        this.remainingBasePayout = 0;
        this.remainingBonusPayout = 0;
    }

    public Date getTimestamp() {
        return this.scanEntry.getTimestamp();
    }

    public Float getDistanceFromArrivalLS() {
        return this.scanEntry.getDistanceFromArrivalLS();
    }

    public String getBodyName() {
        return this.scanEntry.getBodyName();
    }

    public boolean isTerraformingCandidate() {
        return "Terraformable".equals(this.scanEntry.getTerraformState());
    }

    public Float getRadius() {
        return this.scanEntry.getRadius();
    }

    public Float getMass() {
        return this.scanEntry.getStellarMass() != null ? this.scanEntry.getStellarMass() : this.scanEntry.getMassEM();
    }

    public Float getSurfaceTemperature() {
        return this.scanEntry.getSurfaceTemperature();
    }

    public Float getSurfaceGravity() {
        return this.scanEntry.getSurfaceGravity();
    }

    public Map<String, Float> getMaterials() {
        return this.scanEntry.getMaterials();
    }

}
