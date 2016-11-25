package borg.edtrading.eddb.reader;

import org.apache.commons.csv.CSVRecord;

/**
 * CSVRecordParser
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 * @param <T>
 */
public interface CSVRecordParser<T> {

    T parse(CSVRecord record);

}
