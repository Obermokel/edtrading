package borg.edtrading.data;

/**
 * Component
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum Component {

    //@formatter:off
    ARMOUR                                            ("Armour"),
    POWER_PLANT                                       ("Power Plant"),
    THRUSTERS                                         ("Thrusters"),
    FRAME_SHIFT_DRIVE                                 ("Frame Shift Drive"),
    LIFE_SUPPORT                                      ("Life Support"),
    POWER_DISTRIBUTOR                                 ("Power Distributor"),
    BEAM_LASER                                        ("Beam Laser"),
    BURST_LASER                                       ("Burst Laser"),
    CANNON                                            ("Cannon"),
    MANIFEST_SCANNER                                  ("Manifest Scanner"),
    CHAFF_LAUNCHER                                    ("Chaff Launcher"),
    ELECTRONIC_COUNTERMEASURE                         ("Electronic Countermeasure"),
    HEAT_SINK_LAUNCHER                                ("Heat Sink Launcher"),
    POINT_DEFENCE                                     ("Point Defence"),
    FRAGMENT_CANNON                                   ("Fragment Cannon"),
    FRAME_SHIFT_WAKE_SCANNER                          ("Frame Shift Wake Scanner"),
    KILL_WARRANT_SCANNER                              ("Kill Warrant Scanner"),
    MINE_LAUNCHER                                     ("Mine Launcher"),
    MISSILE_RACK                                      ("Missile Rack"),
    MULTICANNON                                       ("Multi-cannon"),
    PLASMA_ACCELERATOR                                ("Plasma Accelerator"),
    PULSE_LASER                                       ("Pulse Laser"),
    RAIL_GUN                                          ("Rail Gun"),
    SHIELD_BOOSTER                                    ("Shield Booster"),
    TORPEDO_PYLON                                     ("Torpedo Pylon"),
    AUTO_FIELDMAINTENANCE_UNIT                        ("Auto Field-Maintenance Unit"),
    COLLECTOR_LIMPET_CONTROLLER                       ("Collector Limpet Controller"),
    FRAME_SHIFT_DRIVE_INTERDICTOR                     ("Frame Shift Drive Interdictor"),
    FUEL_SCOOP                                        ("Fuel Scoop"),
    FUEL_TRANSFER_LIMPET_CONTROLLER                   ("Fuel Transfer Limpet Controller"),
    HATCH_BREAKER_LIMPET_CONTROLLER                   ("Hatch Breaker Limpet Controller"),
    HULL_REINFORCEMENT_PACKAGE                        ("Hull Reinforcement Package"),
    PROSPECTOR_LIMPET_CONTROLLER                      ("Prospector Limpet Controller"),
    REFINERY                                          ("Refinery"),
    SHIELD_CELL_BANK                                  ("Shield Cell Bank"),
    SHIELD_GENERATOR                                  ("Shield Generator");
    //@formatter:on

    private final String name;

    private Component(String name) {
        this.name = name;
    }

    public String getName() {
        return this.name;
    }

}
