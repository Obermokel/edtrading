package borg.edtrading.sidepanel;

/**
 * TravelHistoryListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface TravelHistoryListener {

    void onLocationChanged();

    void onFuelLevelChanged(float newFuelLevel);

}
