package borg.edtrading.sidepanel;

/**
 * TravelHistoryListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface TransactionsListener {

    void onTransactionAdded(Transaction transaction);

    void onTransactionRemoved(Transaction transaction);

}
