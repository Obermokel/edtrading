package borg.edtrading.sidepanel;

/**
 * GameSessionListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface GameSessionListener {

    void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship);

    void onShipModuleChanged(ShipModule oldModule, ShipModule newModule);

    void onShipChanged(ShipLoadout oldShip, ShipLoadout newShip);

}
