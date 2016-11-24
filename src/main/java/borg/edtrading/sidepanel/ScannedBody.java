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

    private static final Map<String, Integer> PAYOUTS = new HashMap<>();
    static {
      //@formatter:off
      PAYOUTS.put(                                "Class O", 3962);//3962
      PAYOUTS.put(                                "Class B", (3490+3329+3307+3055)/4);//3490+3329+3307+3055
      PAYOUTS.put(                                "Class A", (2985+2954)/2);//2985+2954
      PAYOUTS.put(                                "Class F", (2935+2933+2953)/3);//2935+2933+2953
      PAYOUTS.put(                                "Class G", (2920+2920+2921+2917)/4);//2920+2920+2921+2917
      PAYOUTS.put(                                "Class K", (2904+2912+2917)/3);//2904+2912+2917
      PAYOUTS.put(                       "Class K_RedGiant", 2900);
      PAYOUTS.put(                                "Class M", (2898+2897+2895+2893)/4);//2898+2897+2895+2893
      PAYOUTS.put(                       "Class M_RedGiant", 2900);
      PAYOUTS.put(                                "Class C", 2902);//2902
      PAYOUTS.put(                                "Class L", 2889);//2889
      PAYOUTS.put(                                "Class Y", 2882);//2882
      PAYOUTS.put(                                "Class T", (2883+2884)/2);//2883+2884
      PAYOUTS.put(                              "Class TTS", (2900+2882)/2);//2900+2882

      PAYOUTS.put(                                "Class N", (44087+44044+43765+43357)/4);//44087+44044+43765+43357
      PAYOUTS.put(                                "Class H", (44721+45177+45367)/3);//44721+45177+45367
      PAYOUTS.put(                               "Class DA", (27370+27020)/2);//27370+27020
      PAYOUTS.put(                               "Class DB", 27000);

      PAYOUTS.put(                          "Ammonia world", 48802);//48802
      PAYOUTS.put(                         "Earthlike body", (71874+65997)/2);//71874+65997
      PAYOUTS.put(                            "Water world", (32881+33112)/2);//32881+33112
      PAYOUTS.put(                            "Water giant", 1519);//1519
      PAYOUTS.put("High metal content body (Terraformable)", (50638+47257+41831+43757)/4);//50638+47257+41831+43757
      PAYOUTS.put(            "Water world (Terraformable)", (65490+63184+65222)/3);//65490+63184+65222

      PAYOUTS.put(      "Gas giant with ammonia based life", 1871);//1871
      PAYOUTS.put(        "Gas giant with water based life", 2063);//2063
      PAYOUTS.put(             "Sudarsky class I gas giant", (2613+4268)/2);//2613+4268
      PAYOUTS.put(            "Sudarsky class II gas giant", 12888);//12888
      PAYOUTS.put(           "Sudarsky class III gas giant", (1819+1393)/2);//1819+1393
      PAYOUTS.put(            "Sudarsky class IV gas giant", (2831+2748)/2);//2831+2748
      PAYOUTS.put(             "Sudarsky class V gas giant", 1865);//1865

      PAYOUTS.put(                "High metal content body", (6532+7567+5222)/3);//6532+7567+5222
      PAYOUTS.put(                               "Icy body", (925+1117+1155+902)/4);//925+1117+1155+902
      PAYOUTS.put(                        "Metal rich body", (12981+9241)/2);//12981+9241
      PAYOUTS.put(                             "Rocky body", (888+865)/2);//888+865
      PAYOUTS.put(                         "Rocky ice body", 1072);//1072
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
            } else if ("DA".equals(e.getStarType()) || "DB".equals(e.getStarType())) {
                return "White dwarf";
            } else if ("TTS".equals(e.getStarType())) {
                return "T-Tauri star";
            } else if ("C".equals(e.getStarType())) {
                return "Carbon star";
            } else if ("L".equals(e.getStarType()) || "T".equals(e.getStarType()) || "Y".equals(e.getStarType())) {
                return "Class " + e.getStarType() + " dwarf";
            } else if ("M_RedGiant".equals(e.getStarType()) || "K_RedGiant".equals(e.getStarType())) {
                return "Class " + e.getStarType().replace("_RedGiant", "") + " red giant";
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

    public void setToPayedOut() {
        this.remainingBasePayout = 0;
        this.remainingBonusPayout = 0;
    }

    public Date getTimestamp() {
        return this.scanEntry.getTimestamp();
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
