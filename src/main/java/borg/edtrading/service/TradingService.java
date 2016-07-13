package borg.edtrading.service;

import borg.edtrading.Constants;
import borg.edtrading.data.TradingData;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

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

    public Date getLastScannedScreenshotDate() {
        File lastScannedScreenshotDateFile = new File(System.getProperty("user.home"), "ED_lastScannedScreenshotDate.txt");
        if (!lastScannedScreenshotDateFile.exists()) {
            return new Date(0);
        } else {
            try {
                return MiscUtil.getAsDate(FileUtils.readFileToString(lastScannedScreenshotDateFile), new Date(0));
            } catch (IOException e) {
                throw new RuntimeException("Failed to read " + lastScannedScreenshotDateFile, e);
            }
        }
    }

    public void setLastScannedScreenshotDate(Date date) {
        File lastScannedScreenshotDateFile = new File(System.getProperty("user.home"), "ED_lastScannedScreenshotDate.txt");
        if (date == null && lastScannedScreenshotDateFile.exists()) {
            lastScannedScreenshotDateFile.delete();
        } else {
            try {
                FileUtils.write(lastScannedScreenshotDateFile, new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date), false);
            } catch (IOException e) {
                throw new RuntimeException("Failed to write " + lastScannedScreenshotDateFile, e);
            }
        }
    }

    /**
     * @return Sorted ascending by date
     */
    public List<File> getUnscannedScreenshotFiles() {
        final Date lastScannedScreenshotDate = this.getLastScannedScreenshotDate();
        File[] screenshotFiles = Constants.SCREENSHOTS_DIR.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().startsWith("elitedangerous64_") && file.getName().endsWith(".png") && file.lastModified() > lastScannedScreenshotDate.getTime();
            }
        });
        List<File> sortableList = new ArrayList<>(Arrays.asList(screenshotFiles));
        Collections.sort(sortableList, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return new Long(f1.lastModified()).compareTo(new Long(f2.lastModified()));
            }
        });
        return sortableList;
    }

    public void scanNewScreenshots() {
        //        List<File> unscannedScreenshotFiles = this.getUnscannedScreenshotFiles();
        //        for (File screenshotFile : unscannedScreenshotFiles) {
        //            Map<Integer, Map<String, String>> data = ScreenshotScanner.scanScreenshot(screenshotFile);
        //
        //            Date timestamp = new Date(screenshotFile.lastModified());
        //            String starSystemName = null; // TODO
        //            StarSystem starSystem = new StarSystem(starSystemName);
        //            String stationName = null; // TODO
        //            double distanceFromStarInLs = 0;
        //            Station station = new Station(stationName, starSystem, distanceFromStarInLs);
        //            Cargo cargo = null; // TODO
        //            Long priceToSell = null; // TODO
        //            Long priceToBuy = null; // TODO
        //
        //            TradingData tradingData = new TradingData(timestamp, station, cargo, priceToSell, priceToBuy);
        //        }
    }

    public void saveTradingData(TradingData tradingData) {
        TradingDao.getInstance().saveCargo(tradingData.getCommodity());
        TradingDao.getInstance().saveStarSystem(tradingData.getStation().getStarSystem());
        TradingDao.getInstance().saveStation(tradingData.getStation());
        TradingDao.getInstance().saveTradingData(tradingData);
    }

}
