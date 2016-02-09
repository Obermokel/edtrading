package borg.edtrading.service;

import borg.edtrading.data.TradingData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * TradingService
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TradingService {

    static final Logger logger = LogManager.getLogger(TradingService.class);

    private static final TradingService INSTANCE = new TradingService();

    private TradingService() {
        // Singleton
    }

    public static TradingService getInstance() {
        return INSTANCE;
    }

    public void saveTradingData(TradingData tradingData) {
        TradingDao.getInstance().saveCargo(tradingData.getCargo());
        TradingDao.getInstance().saveStarSystem(tradingData.getStation().getStarSystem());
        TradingDao.getInstance().saveStation(tradingData.getStation());
        TradingDao.getInstance().saveTradingData(tradingData);
    }

}
