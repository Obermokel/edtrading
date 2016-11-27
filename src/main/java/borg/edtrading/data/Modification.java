package borg.edtrading.data;

/**
 * Modification
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum Modification {

    //@formatter:off
    BLAST_RESISTANT_ARMOUR                            ("Blast Resistant Armour"),
    HEAVY_DUTY_ARMOUR                                 ("Heavy Duty Armour"),
    KINETIC_RESISTANT_ARMOUR                          ("Kinetic Resistant Armour"),
    LIGHTWEIGHT_ARMOUR                                ("Lightweight Armour"),
    THERMAL_RESISTANT_ARMOUR                          ("Thermal Resistant Armour"),
    ARMOURED_POWER_PLANT                              ("Armoured Power Plant"),
    LOW_EMISSIONS_POWER_PLANT                         ("Low Emissions Power Plant"),
    OVERCHARGED_POWER_PLANT                           ("Overcharged Power Plant"),
    CLEAN_DRIVE_TUNING                                ("Clean Drive Tuning"),
    DIRTY_DRIVE_TUNING                                ("Dirty Drive Tuning"),
    DRIVE_STRENGTHENING                               ("Drive Strengthening"),
    FASTER_FSD_BOOT_SEQUENCE                          ("Faster FSD Boot Sequence"),
    INCREASED_FSD_RANGE                               ("Increased FSD Range"),
    SHIELDED_FSD                                      ("Shielded FSD"),
    LIGHTWEIGHT                                       ("Lightweight"),
    REINFORCED                                        ("Reinforced"),
    SHIELDED                                          ("Shielded"),
    CHARGE_ENHANCED_POWER_DISTRIBUTOR                 ("Charge Enhanced Power Distributor"),
    ENGINE_FOCUSED_POWER_DISTRIBUTOR                  ("Engine Focused Power Distributor"),
    HIGH_CHARGE_CAPACITY_POWER_DISTRIBUTOR            ("High Charge Capacity Power Distributor"),
    SHIELDED_POWER_DISTRIBUTOR                        ("Shielded Power Distributor"),
    SYSTEM_FOCUSED_POWER_DISTRIBUTOR                  ("System Focused Power Distributor"),
    WEAPON_FOCUSED_POWER_DISTRIBUTOR                  ("Weapon Focused Power Distributor"),
    LIGHTWEIGHT_MOUNT                                 ("Lightweight Mount"),
    LONG_RANGE_WEAPON                                 ("Long Range Weapon"),
    SHORT_RANGE_BLASTER                               ("Short Range Blaster"),
    STURDY_MOUNT                                      ("Sturdy Mount"),
    EFFICIENT_WEAPON                                  ("Efficient Weapon"),
    FOCUSED_WEAPON                                    ("Focused Weapon"),
    OVERCHARGED_WEAPON                                ("Overcharged Weapon"),
    RAPID_FIRE_MODIFICATION                           ("Rapid Fire Modification"),
    HIGH_CAPACITY_MAGAZINE                            ("High Capacity Magazine"),
    AMMO_CAPACITY                                     ("Ammo Capacity"),
    DOUBLE_SHOT                                       ("Double Shot"),
    BLAST_RESISTANT_SHIELD_BOOSTER                    ("Blast Resistant Shield Booster"),
    KINETIC_RESISTANT_SHIELD_BOOSTER                  ("Kinetic Resistant Shield Booster"),
    RESISTANCE_AUGMENTED_SHIELD_BOOSTER               ("Resistance Augmented Shield Booster"),
    THERMAL_RESISTANT_SHIELD_BOOSTER                  ("Thermal Resistant Shield Booster"),
    EXPANDED_FSD_INTERDICTOR_CAPTURE_ARC              ("Expanded FSD Interdictor Capture Arc"),
    LONGER_RANGE_FSD_INTERDICTOR                      ("Longer Range FSD interdictor"),
    BLAST_RESISTANT_HULL_REINFORCEMENT                ("Blast Resistant Hull Reinforcement"),
    HEAVY_DUTY_HULL_REINFORCEMENT                     ("Heavy Duty Hull Reinforcement"),
    KINETIC_RESISTANT_HULL_REINFORCEMENT              ("Kinetic Resistant Hull Reinforcement"),
    LIGHTWEIGHT_HULL_REINFORCEMENT                    ("Lightweight Hull Reinforcement"),
    THERMAL_RESISTANT_HULL_REINFORCEMENT              ("Thermal Resistant Hull Reinforcement"),
    RAPID_CHARGE_SHIELD_CELL_BANK                     ("Rapid Charge Shield Cell Bank"),
    SPECIALISED_SHIELD_CELL_BANK                      ("Specialised Shield Cell Bank"),
    ENHANCED_LOW_POWER_SHIELDS                        ("Enhanced, Low Power Shields"),
    KINETIC_RESISTANT_SHIELDS                         ("Kinetic Resistant Shields"),
    REINFORCED_SHIELDS                                ("Reinforced Shields"),
    THERMAL_RESISTANT_SHIELDS                         ("Thermal Resistant Shields");
    //@formatter:on

    private final String name;

    private Modification(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.getName();
    }

    public String getName() {
        return this.name;
    }

}
