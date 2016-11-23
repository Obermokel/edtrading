package borg.edtrading.sidepanel;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.JournalUpdateListener;
import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.missions.CommunityGoalJoinEntry;
import borg.edtrading.journal.entries.missions.CommunityGoalRewardEntry;
import borg.edtrading.journal.entries.missions.MissionAcceptedEntry;
import borg.edtrading.journal.entries.missions.MissionCompletedEntry;
import borg.edtrading.sidepanel.Transaction.TransactionType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Transactions
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Transactions implements JournalUpdateListener, Serializable {

    private static final long serialVersionUID = 4164678390231078712L;

    static final Logger logger = LogManager.getLogger(Transactions.class);

    private final List<Transaction> transactions = new ArrayList<>();

    private final List<TransactionsListener> listeners = new ArrayList<>();

    public Transactions(JournalReaderThread journalReaderThread) {
        if (journalReaderThread != null) {
            journalReaderThread.addListener(this);
        }
    }

    @Override
    public void onNewJournalLine(String line) {
        // Do nothing
    }

    @Override
    public void onNewJournalEntry(AbstractJournalEntry entry) {
        try {
            // TODO claims/bonds, bounties/fines
            if (entry.getEvent() == Event.MissionAccepted) {
                this.addTransaction(new Transaction((MissionAcceptedEntry) entry));
            } else if (entry.getEvent() == Event.MissionCompleted) {
                this.removeTransaction(new Transaction((MissionCompletedEntry) entry));
            } else if (entry.getEvent() == Event.CommunityGoalJoin) {
                this.addTransaction(new Transaction((CommunityGoalJoinEntry) entry));
            } else if (entry.getEvent() == Event.CommunityGoalReward) {
                this.removeTransaction(new Transaction((CommunityGoalRewardEntry) entry));
            }
        } catch (Exception e) {
            logger.error("Failed to handle " + entry, e);
        }
    }

    public List<Transaction> getTransactions() {
        return new ArrayList<>(this.transactions);
    }

    public List<Transaction> getTransactions(TransactionType type) {
        return new ArrayList<>(this.transactions.stream().filter(t -> t.getType() == type).collect(Collectors.toList()));
    }

    private void addTransaction(Transaction transaction) {
        if (transaction != null) {
            this.transactions.add(transaction);

            for (TransactionsListener listener : this.listeners) {
                try {
                    listener.onTransactionAdded(transaction);
                } catch (Exception ex) {
                    logger.warn(listener + " failed: " + ex);
                }
            }
        }
    }

    private void removeTransaction(Transaction transaction) {
        if (transaction.getType() == TransactionType.MISSION) {
            Transaction missionAcceptedTransaction = this.getTransactions(TransactionType.MISSION).stream().filter(t -> t.getMissionID().equals(transaction.getMissionID())).findFirst().get();
            if (missionAcceptedTransaction != null) {
                this.transactions.remove(missionAcceptedTransaction);

                for (TransactionsListener listener : this.listeners) {
                    try {
                        listener.onTransactionRemoved(missionAcceptedTransaction);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            }
        } else if (transaction.getType() == TransactionType.COMMUNITY_GOAL) {
            Transaction communityGoalJoinTransaction = this.getTransactions(TransactionType.COMMUNITY_GOAL).stream().filter(t -> t.getName().equals(transaction.getName())).findFirst().get();
            if (communityGoalJoinTransaction != null) {
                this.transactions.remove(communityGoalJoinTransaction);

                for (TransactionsListener listener : this.listeners) {
                    try {
                        listener.onTransactionRemoved(communityGoalJoinTransaction);
                    } catch (Exception ex) {
                        logger.warn(listener + " failed: " + ex);
                    }
                }
            }
        }
    }

    public boolean addListener(TransactionsListener listener) {
        if (listener == null || this.listeners.contains(listener)) {
            return false;
        } else {
            return this.listeners.add(listener);
        }
    }

    public boolean removeListener(TransactionsListener listener) {
        if (listener == null) {
            return false;
        } else {
            return this.listeners.remove(listener);
        }
    }

}
