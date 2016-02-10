package borg.edtrading;

import java.sql.Timestamp;

import borg.edtrading.data.Cargo;
import borg.edtrading.data.StarSystem;
import borg.edtrading.data.Station;
import borg.edtrading.data.TradingData;
import borg.edtrading.service.TradingService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Stupido
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Stupido {

    static final Logger logger = LogManager.getLogger(Stupido.class);

    public static void main(String[] args) {
        final Timestamp now = new Timestamp(System.currentTimeMillis());
        final Cargo erntemaschinen = new Cargo("ERNTEMASCHINEN", 2378L);
        final StarSystem bandua = new StarSystem("BANDUA");
        final Station gagnanMarket = new Station("GAGNAN MARKET", bandua, 859.77);
        final StarSystem olelbis = new StarSystem("OLELBIS");
        final Station polyakovOrbital = new Station("POLYAKOV ORBITAL", olelbis, 452.24);

        TradingData tdGagnanEm = new TradingData(now, gagnanMarket, erntemaschinen, 2404L, null);
        TradingData tdPolyakovEm = new TradingData(now, polyakovOrbital, erntemaschinen, 1928L, 1953L);

        TradingService.getInstance().saveTradingData(tdGagnanEm);
        TradingService.getInstance().saveTradingData(tdPolyakovEm);
    }

}
