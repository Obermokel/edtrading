package borg.edtrading.journal;

import borg.edtrading.journal.entries.AbstractJournalEntry;
import borg.edtrading.journal.entries.comms.ReceiveTextEntry;
import borg.edtrading.journal.entries.comms.SendTextEntry;
import borg.edtrading.journal.entries.engineer.EngineerApplyEntry;
import borg.edtrading.journal.entries.engineer.EngineerCraftEntry;
import borg.edtrading.journal.entries.engineer.EngineerProgressEntry;
import borg.edtrading.journal.entries.exploration.ScanEntry;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.journal.entries.fight.BountyEntry;
import borg.edtrading.journal.entries.fight.CockpitBreachedEntry;
import borg.edtrading.journal.entries.fight.CommitCrimeEntry;
import borg.edtrading.journal.entries.fight.DiedEntry;
import borg.edtrading.journal.entries.fight.EscapeInterdictionEntry;
import borg.edtrading.journal.entries.fight.FactionKillBondEntry;
import borg.edtrading.journal.entries.fight.HeatDamageEntry;
import borg.edtrading.journal.entries.fight.HeatWarningEntry;
import borg.edtrading.journal.entries.fight.HullDamageEntry;
import borg.edtrading.journal.entries.fight.InterdictedEntry;
import borg.edtrading.journal.entries.fight.RebootRepairEntry;
import borg.edtrading.journal.entries.fight.ResurrectEntry;
import borg.edtrading.journal.entries.fight.ShieldStateEntry;
import borg.edtrading.journal.entries.fleet.FetchRemoteModuleEntry;
import borg.edtrading.journal.entries.fleet.ModuleBuyEntry;
import borg.edtrading.journal.entries.fleet.ModuleRetrieveEntry;
import borg.edtrading.journal.entries.fleet.ModuleSellEntry;
import borg.edtrading.journal.entries.fleet.ModuleSellRemoteEntry;
import borg.edtrading.journal.entries.fleet.ModuleStoreEntry;
import borg.edtrading.journal.entries.fleet.ModuleSwapEntry;
import borg.edtrading.journal.entries.fleet.ShipyardBuyEntry;
import borg.edtrading.journal.entries.fleet.ShipyardNewEntry;
import borg.edtrading.journal.entries.fleet.ShipyardSwapEntry;
import borg.edtrading.journal.entries.fleet.ShipyardTransferEntry;
import borg.edtrading.journal.entries.game.FileheaderEntry;
import borg.edtrading.journal.entries.game.LoadGameEntry;
import borg.edtrading.journal.entries.game.ProgressEntry;
import borg.edtrading.journal.entries.game.RankEntry;
import borg.edtrading.journal.entries.game.ScreenshotEntry;
import borg.edtrading.journal.entries.inventory.BuyDronesEntry;
import borg.edtrading.journal.entries.inventory.CollectCargoEntry;
import borg.edtrading.journal.entries.inventory.EjectCargoEntry;
import borg.edtrading.journal.entries.inventory.MaterialCollectedEntry;
import borg.edtrading.journal.entries.inventory.MaterialDiscardedEntry;
import borg.edtrading.journal.entries.inventory.MaterialDiscoveredEntry;
import borg.edtrading.journal.entries.inventory.MiningRefinedEntry;
import borg.edtrading.journal.entries.inventory.ScientificResearchEntry;
import borg.edtrading.journal.entries.inventory.SellDronesEntry;
import borg.edtrading.journal.entries.inventory.SynthesisEntry;
import borg.edtrading.journal.entries.location.ApproachSettlementEntry;
import borg.edtrading.journal.entries.location.DockedEntry;
import borg.edtrading.journal.entries.location.FSDJumpEntry;
import borg.edtrading.journal.entries.location.LiftoffEntry;
import borg.edtrading.journal.entries.location.LocationEntry;
import borg.edtrading.journal.entries.location.SupercruiseEntryEntry;
import borg.edtrading.journal.entries.location.SupercruiseExitEntry;
import borg.edtrading.journal.entries.location.TouchdownEntry;
import borg.edtrading.journal.entries.location.USSDropEntry;
import borg.edtrading.journal.entries.location.UndockedEntry;
import borg.edtrading.journal.entries.missions.CommunityGoalJoinEntry;
import borg.edtrading.journal.entries.missions.CommunityGoalRewardEntry;
import borg.edtrading.journal.entries.missions.DataScannedEntry;
import borg.edtrading.journal.entries.missions.MissionAcceptedEntry;
import borg.edtrading.journal.entries.missions.MissionCompletedEntry;
import borg.edtrading.journal.entries.slf.DockFighterEntry;
import borg.edtrading.journal.entries.slf.LaunchFighterEntry;
import borg.edtrading.journal.entries.slf.VehicleSwitchEntry;
import borg.edtrading.journal.entries.srv.DockSRVEntry;
import borg.edtrading.journal.entries.srv.LaunchSRVEntry;
import borg.edtrading.journal.entries.starport.BuyAmmoEntry;
import borg.edtrading.journal.entries.starport.CrewAssignEntry;
import borg.edtrading.journal.entries.starport.CrewFireEntry;
import borg.edtrading.journal.entries.starport.CrewHireEntry;
import borg.edtrading.journal.entries.starport.DockingDeniedEntry;
import borg.edtrading.journal.entries.starport.DockingGrantedEntry;
import borg.edtrading.journal.entries.starport.DockingRequestedEntry;
import borg.edtrading.journal.entries.starport.MarketBuyEntry;
import borg.edtrading.journal.entries.starport.MarketSellEntry;
import borg.edtrading.journal.entries.starport.PayFinesEntry;
import borg.edtrading.journal.entries.starport.PayLegacyFinesEntry;
import borg.edtrading.journal.entries.starport.RedeemVoucherEntry;
import borg.edtrading.journal.entries.starport.RefuelAllEntry;
import borg.edtrading.journal.entries.starport.RepairAllEntry;
import borg.edtrading.journal.entries.starport.RepairEntry;
import borg.edtrading.journal.entries.starport.RestockVehicleEntry;
import borg.edtrading.journal.entries.travel.FuelScoopEntry;
import borg.edtrading.journal.entries.travel.JetConeBoostEntry;
import borg.edtrading.journal.entries.travel.JetConeDamageEntry;
import borg.edtrading.journal.entries.wing.WingAddEntry;
import borg.edtrading.journal.entries.wing.WingJoinEntry;
import borg.edtrading.journal.entries.wing.WingLeaveEntry;
import borg.edtrading.util.MiscUtil;
import com.google.gson.Gson;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.lang3.StringUtils;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

/**
 * Not thread safe
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class JournalReader {

    static final Logger logger = LogManager.getLogger(JournalReader.class);

    private final DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
    private final Gson gson = new Gson();

    public List<AbstractJournalEntry> readEntireJournal(File journalDir) throws IOException {
        List<AbstractJournalEntry> result = new ArrayList<>();

        File[] journalFiles = journalDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().startsWith("Journal.") && file.getName().endsWith(".log");
            }
        });
        Arrays.sort(journalFiles, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return new Long(f1.lastModified()).compareTo(new Long(f2.lastModified()));
            }
        });

        LinkedHashMap<String, Integer> unknownEventCounts = new LinkedHashMap<>();
        for (File journalFile : journalFiles) {
            result.addAll(this.readJournalFile(journalFile, unknownEventCounts));
        }

        MiscUtil.sortMapByValue(unknownEventCounts);
        for (String eventName : unknownEventCounts.keySet()) {
            logger.debug(String.format(Locale.US, "Unknown event: %4dx %s", unknownEventCounts.get(eventName), eventName));
        }

        return result;
    }

    public List<AbstractJournalEntry> readJournalFile(File journalFile, LinkedHashMap<String, Integer> unknownEventCounts) throws IOException {
        List<AbstractJournalEntry> result = new ArrayList<>();

        List<String> lines = FileUtils.readLines(journalFile, "UTF-8");
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            try {
                AbstractJournalEntry entry = this.readJournalLine(line);
                if (entry != null) {
                    result.add(entry);
                }
            } catch (UnknownEventException e) {
                logger.trace("Unknown event type '" + e.getEvent() + "' in line " + (i + 1) + " of " + journalFile);
                Integer count = unknownEventCounts.get(e.getEvent());
                if (count == null) {
                    count = new Integer(0);
                    unknownEventCounts.put(e.getEvent(), count);
                }
                unknownEventCounts.put(e.getEvent(), unknownEventCounts.get(e.getEvent()) + 1);
            } catch (Exception e) {
                throw new RuntimeException("Failed to parse line " + (i + 1) + " of " + journalFile + ": " + line, e);
            }
        }

        return result;
    }

    /**
     * @return Can return <code>null</code>
     */
    public AbstractJournalEntry readJournalLine(String line) throws UnknownEventException {
        if (StringUtils.isEmpty(line)) {
            return null;
        } else {
            LinkedHashMap<String, Object> data = this.gson.fromJson(line, LinkedHashMap.class);
            AbstractJournalEntry entry = this.readJournalData(data);
            if (!data.isEmpty()) {
                logger.debug("Unknown " + entry.getEvent() + " data: " + data);
            }
            return entry;
        }
    }

    /**
     * @return Can return <code>null</code>
     */
    private AbstractJournalEntry readJournalData(LinkedHashMap<String, Object> data) throws UnknownEventException {
        if (data == null || data.isEmpty()) {
            return null;
        } else {
            String timestampValue = (String) data.remove("timestamp");
            Date timestamp = null;
            try {
                timestamp = this.df.parse(timestampValue);
            } catch (ParseException e) {
                throw new RuntimeException("Failed to parse timestamp '" + timestampValue + "'", e);
            }
            String eventName = (String) data.remove("event");
            Event event = null;
            try {
                event = Event.valueOf(eventName);
            } catch (IllegalArgumentException e) {
                throw new UnknownEventException(eventName);
            }

            switch (event) {
                case ApproachSettlement:
                    return new ApproachSettlementEntry(timestamp, event, data);
                case Bounty:
                    return new BountyEntry(timestamp, event, data);
                case BuyAmmo:
                    return new BuyAmmoEntry(timestamp, event, data);
                case BuyDrones:
                    return new BuyDronesEntry(timestamp, event, data);
                case CollectCargo:
                    return new CollectCargoEntry(timestamp, event, data);
                case CockpitBreached:
                    return new CockpitBreachedEntry(timestamp, event, data);
                case CommitCrime:
                    return new CommitCrimeEntry(timestamp, event, data);
                case CommunityGoalJoin:
                    return new CommunityGoalJoinEntry(timestamp, event, data);
                case CommunityGoalReward:
                    return new CommunityGoalRewardEntry(timestamp, event, data);
                case CrewAssign:
                    return new CrewAssignEntry(timestamp, event, data);
                case CrewFire:
                    return new CrewFireEntry(timestamp, event, data);
                case CrewHire:
                    return new CrewHireEntry(timestamp, event, data);
                case DataScanned:
                    return new DataScannedEntry(timestamp, event, data);
                case Died:
                    return new DiedEntry(timestamp, event, data);
                case Docked:
                    return new DockedEntry(timestamp, event, data);
                case DockingDenied:
                    return new DockingDeniedEntry(timestamp, event, data);
                case DockingGranted:
                    return new DockingGrantedEntry(timestamp, event, data);
                case DockingRequested:
                    return new DockingRequestedEntry(timestamp, event, data);
                case DockFighter:
                    return new DockFighterEntry(timestamp, event, data);
                case DockSRV:
                    return new DockSRVEntry(timestamp, event, data);
                case EjectCargo:
                    return new EjectCargoEntry(timestamp, event, data);
                case EngineerApply:
                    return new EngineerApplyEntry(timestamp, event, data);
                case EngineerCraft:
                    return new EngineerCraftEntry(timestamp, event, data);
                case EngineerProgress:
                    return new EngineerProgressEntry(timestamp, event, data);
                case EscapeInterdiction:
                    return new EscapeInterdictionEntry(timestamp, event, data);
                case FactionKillBond:
                    return new FactionKillBondEntry(timestamp, event, data);
                case FetchRemoteModule:
                    return new FetchRemoteModuleEntry(timestamp, event, data);
                case Fileheader:
                    return new FileheaderEntry(timestamp, event, data);
                case FSDJump:
                    return new FSDJumpEntry(timestamp, event, data);
                case FuelScoop:
                    return new FuelScoopEntry(timestamp, event, data);
                case HeatDamage:
                    return new HeatDamageEntry(timestamp, event, data);
                case HeatWarning:
                    return new HeatWarningEntry(timestamp, event, data);
                case HullDamage:
                    return new HullDamageEntry(timestamp, event, data);
                case Interdicted:
                    return new InterdictedEntry(timestamp, event, data);
                case JetConeBoost:
                    return new JetConeBoostEntry(timestamp, event, data);
                case JetConeDamage:
                    return new JetConeDamageEntry(timestamp, event, data);
                case LaunchFighter:
                    return new LaunchFighterEntry(timestamp, event, data);
                case LaunchSRV:
                    return new LaunchSRVEntry(timestamp, event, data);
                case Liftoff:
                    return new LiftoffEntry(timestamp, event, data);
                case LoadGame:
                    return new LoadGameEntry(timestamp, event, data);
                case Location:
                    return new LocationEntry(timestamp, event, data);
                case MarketBuy:
                    return new MarketBuyEntry(timestamp, event, data);
                case MarketSell:
                    return new MarketSellEntry(timestamp, event, data);
                case MaterialCollected:
                    return new MaterialCollectedEntry(timestamp, event, data);
                case MaterialDiscarded:
                    return new MaterialDiscardedEntry(timestamp, event, data);
                case MaterialDiscovered:
                    return new MaterialDiscoveredEntry(timestamp, event, data);
                case MiningRefined:
                    return new MiningRefinedEntry(timestamp, event, data);
                case MissionAccepted:
                    return new MissionAcceptedEntry(timestamp, event, data);
                case MissionCompleted:
                    return new MissionCompletedEntry(timestamp, event, data);
                case ModuleBuy:
                    return new ModuleBuyEntry(timestamp, event, data);
                case ModuleRetrieve:
                    return new ModuleRetrieveEntry(timestamp, event, data);
                case ModuleSell:
                    return new ModuleSellEntry(timestamp, event, data);
                case ModuleSellRemote:
                    return new ModuleSellRemoteEntry(timestamp, event, data);
                case ModuleStore:
                    return new ModuleStoreEntry(timestamp, event, data);
                case ModuleSwap:
                    return new ModuleSwapEntry(timestamp, event, data);
                case PayFines:
                    return new PayFinesEntry(timestamp, event, data);
                case PayLegacyFines:
                    return new PayLegacyFinesEntry(timestamp, event, data);
                case Progress:
                    return new ProgressEntry(timestamp, event, data);
                case Rank:
                    return new RankEntry(timestamp, event, data);
                case RebootRepair:
                    return new RebootRepairEntry(timestamp, event, data);
                case ReceiveText:
                    return new ReceiveTextEntry(timestamp, event, data);
                case RedeemVoucher:
                    return new RedeemVoucherEntry(timestamp, event, data);
                case RefuelAll:
                    return new RefuelAllEntry(timestamp, event, data);
                case Repair:
                    return new RepairEntry(timestamp, event, data);
                case RepairAll:
                    return new RepairAllEntry(timestamp, event, data);
                case RestockVehicle:
                    return new RestockVehicleEntry(timestamp, event, data);
                case Resurrect:
                    return new ResurrectEntry(timestamp, event, data);
                case Scan:
                    return new ScanEntry(timestamp, event, data);
                case ScientificResearch:
                    return new ScientificResearchEntry(timestamp, event, data);
                case Screenshot:
                    return new ScreenshotEntry(timestamp, event, data);
                case SellDrones:
                    return new SellDronesEntry(timestamp, event, data);
                case SellExplorationData:
                    return new SellExplorationDataEntry(timestamp, event, data);
                case SendText:
                    return new SendTextEntry(timestamp, event, data);
                case ShieldState:
                    return new ShieldStateEntry(timestamp, event, data);
                case ShipyardBuy:
                    return new ShipyardBuyEntry(timestamp, event, data);
                case ShipyardNew:
                    return new ShipyardNewEntry(timestamp, event, data);
                case ShipyardTransfer:
                    return new ShipyardTransferEntry(timestamp, event, data);
                case ShipyardSwap:
                    return new ShipyardSwapEntry(timestamp, event, data);
                case SupercruiseEntry:
                    return new SupercruiseEntryEntry(timestamp, event, data);
                case SupercruiseExit:
                    return new SupercruiseExitEntry(timestamp, event, data);
                case Synthesis:
                    return new SynthesisEntry(timestamp, event, data);
                case Touchdown:
                    return new TouchdownEntry(timestamp, event, data);
                case Undocked:
                    return new UndockedEntry(timestamp, event, data);
                case USSDrop:
                    return new USSDropEntry(timestamp, event, data);
                case VehicleSwitch:
                    return new VehicleSwitchEntry(timestamp, event, data);
                case WingAdd:
                    return new WingAddEntry(timestamp, event, data);
                case WingJoin:
                    return new WingJoinEntry(timestamp, event, data);
                case WingLeave:
                    return new WingLeaveEntry(timestamp, event, data);
                default:
                    logger.error("Unhandled event in switch clause: " + event);
                    return null;
            }
        }
    }

}
