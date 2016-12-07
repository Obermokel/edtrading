package borg.edtrading;

import borg.edtrading.data.Item;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedInputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * InaraEngineeringDownloader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InaraEngineeringDownloader {

    static final Logger logger = LogManager.getLogger(InaraEngineeringDownloader.class);

    Map<String, InaraItem> knownItems = new TreeMap<>();
    Map<String, InaraEngineer> knownEngineers = new TreeMap<>();
    List<InaraBlueprint> knownBlueprints = new ArrayList<>();

    public static void main(String[] args) throws Exception {
        InaraEngineeringDownloader downloader = new InaraEngineeringDownloader();
        downloader.download();
    }

    void download() throws Exception {
        this.readItems();
        this.readEngineers();
        this.readBlueprints();
        this.printEnum‬SourcecodeForCopyAndPaste();
    }

    void printEnum‬SourcecodeForCopyAndPaste() {
        System.out.println("==== Item.java ====");
        Map<String, List<InaraItem>> itemsByType = this.knownItems.values().stream().collect(Collectors.groupingBy(InaraItem::getType));
        for (String type : itemsByType.keySet()) {
            List<InaraItem> items = itemsByType.get(type).stream().sorted((i1, i2) -> i2.getName().toLowerCase().compareTo(i2.getName().toLowerCase())).collect(Collectors.toList());
            String itemType = type.replace("Raw material", "Element").toUpperCase();
            System.out.println("    // " + itemType);
            for (InaraItem item : items) {
                Item currentlyKnown = Item.findBestMatching(item.getName(), null);
                String enumConstant = item.getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "");
                String name = "\"" + item.getName() + "\"";
                String journalName = currentlyKnown == null || StringUtils.isEmpty(currentlyKnown.getJournalName()) ? null : "\"" + currentlyKnown.getJournalName() + "\"";
                String typeEnum = "ItemType." + itemType;
                int grade = item.getGrade();
                System.out.println(String.format(Locale.US, "    %-50s(%-50s, %-50s, %-25s, %d),", enumConstant, name, journalName, typeEnum, grade));
            }
            System.out.println();
        }
        System.out.println("\n\n\n");

        System.out.println("==== Engineer.java ====");
        for (InaraEngineer engineer : this.knownEngineers.values()) {
            String enumConstant = engineer.getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "");
            String name = "\"" + engineer.getName().replace("\"", "\\\"") + "\"";
            String system = "\"" + engineer.getSystem() + "\"";
            String base = "\"" + engineer.getBase() + "\"";
            System.out.println(String.format(Locale.US, "    %-50s(%-50s, %-50s, %s),", enumConstant, name, system, base));
        }
        System.out.println("\n\n\n");

        System.out.println("==== Component.java ====");
        List<InaraComponent> components = this.knownBlueprints.stream().map(InaraBlueprint::getComponent).distinct().sorted((i1, i2) -> i2.getName().toLowerCase().compareTo(i2.getName().toLowerCase())).collect(Collectors.toList());
        for (InaraComponent component : components) {
            String enumConstant = component.getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "");
            String name = "\"" + component.getName() + "\"";
            System.out.println(String.format(Locale.US, "    %-50s(%s),", enumConstant, name));
        }
        System.out.println("\n\n\n");

        System.out.println("==== Modification.java ====");
        List<InaraModification> modifications = this.knownBlueprints.stream().map(InaraBlueprint::getModification).distinct().sorted((i1, i2) -> i2.getName().toLowerCase().compareTo(i2.getName().toLowerCase())).collect(Collectors.toList());
        for (InaraModification modification : modifications) {
            String enumConstant = modification.getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "");
            String name = "\"" + modification.getName() + "\"";
            System.out.println(String.format(Locale.US, "    %-50s(%s),", enumConstant, name));
        }
        System.out.println("\n\n\n");

        System.out.println("==== Blueprint.java ====");
        for (InaraBlueprint blueprint : this.knownBlueprints) {
            int grade = blueprint.getGrade();
            String enumConstant = (blueprint.getComponent().getName() + "___" + blueprint.getModification().getName() + "___GRADE_" + grade).toUpperCase().replace(" ", "_").replaceAll("\\W", "");
            String componentEnum = blueprint.getComponent().getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "");
            String modificationEnum = blueprint.getModification().getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "");
            String ingBuilder = "new IngredientsBuilder()";
            for (InaraItem item : blueprint.getIngredients().keySet()) {
                int count = blueprint.getIngredients().get(item);
                ingBuilder += (".add(Item." + item.getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "") + ", " + count + ")");
            }
            ingBuilder += ".build()";
            String engBuilder = "new EngineersBuilder()";
            for (InaraEngineer engineer : blueprint.getEngineers()) {
                engBuilder += (".add(Engineer." + engineer.getName().toUpperCase().replace(" ", "_").replaceAll("\\W", "") + ")");
            }
            engBuilder += ".build()";
            System.out.println(String.format(Locale.US, "    %-80s(Component.%-40s, Modification.%-40s, %d, %s, %s),", enumConstant, componentEnum, modificationEnum, grade, ingBuilder, engBuilder));
        }
        System.out.println("\n\n\n");
    }

    void readItems() throws Exception {
        String html = readUrl("http://inara.cz/galaxy-components");

        int tableIndex = html.indexOf("DataTables_Table_0");
        int tbodyIndex = html.indexOf("<tbody>", tableIndex);
        int trOpenIndex = html.indexOf("<tr>", tbodyIndex);
        int trCloseIndex = html.indexOf("</tr>", trOpenIndex);
        while (trCloseIndex > trOpenIndex && trOpenIndex >= 0) {
            String tr = html.substring(trOpenIndex, trCloseIndex);
            // 1: Item name
            // 2: Item type
            // 3: Grade
            Pattern p = Pattern.compile(".*>(.+)</a></td>.+>(.+)</td>.+grade(\\-?\\d)\\.png.*");
            Matcher m = p.matcher(tr);
            if (m.matches()) {
                String itemName = m.group(1);
                String itemType = m.group(2);
                String grade = m.group(3);
                this.knownItems.put(itemName, new InaraItem(itemName, itemType, Integer.valueOf(grade)));
            }
            trOpenIndex = html.indexOf("<tr>", trCloseIndex);
            trCloseIndex = html.indexOf("</tr>", trOpenIndex);
        }
    }

    void readEngineers() throws Exception {
        String html = readUrl("http://inara.cz/galaxy-engineers");
        Pattern p = Pattern.compile("href=\"/galaxy\\-engineer/(\\d+)\"");
        Matcher m = p.matcher(html);
        while (m.find()) {
            this.readEngineer("http://inara.cz/galaxy-engineer/" + m.group(1));
        }
    }

    void readEngineer(String url) throws Exception {
        String html = readUrl(url);
        int nameStart = html.indexOf("<h2");
        int nameEnd = html.indexOf("</h2>", nameStart);
        String engineerName = html.substring(nameStart, nameEnd);
        engineerName = engineerName.substring(engineerName.lastIndexOf(">") + 1).trim();
        int locationStart = html.indexOf("Location:</div>", nameEnd);
        int locationEnd = html.indexOf("</span></a><br>", locationStart);
        String location = html.substring(locationStart, locationEnd);
        Pattern p = Pattern.compile(".*?class=\"normal\">(.+)\\|.+class=\"uppercase\">(.+).*?");
        Matcher m = p.matcher(location);
        if (m.matches()) {
            String base = m.group(1).trim();
            String system = m.group(2).trim();
            this.knownEngineers.put(engineerName, new InaraEngineer(engineerName, system, base));
        } else {
            this.knownEngineers.put(engineerName, new InaraEngineer(engineerName, "", ""));
        }
    }

    void readBlueprints() throws Exception {
        String html = readUrl("http://inara.cz/galaxy-blueprints");

        int tableIndex = html.indexOf("DataTables_Table_0");
        int tbodyIndex = html.indexOf("<tbody>", tableIndex);
        int trOpenIndex = html.indexOf("<tr>", tbodyIndex);
        int trCloseIndex = html.indexOf("</tr>", trOpenIndex);
        while (trCloseIndex > trOpenIndex && trOpenIndex >= 0) {
            String tr = html.substring(trOpenIndex, trCloseIndex);
            // 1: Component name
            // 2: Path
            // 3: Modification name
            Pattern p = Pattern.compile(".*>(.+)</td>.+href=\"(.+)\" class.+>(.+)</a>.*");
            Matcher m = p.matcher(tr);
            if (m.matches()) {
                String componentName = m.group(1);
                String modificationName = m.group(3);
                String path = m.group(2);
                this.readBlueprints(componentName, modificationName, "http://inara.cz" + path);
            }
            trOpenIndex = html.indexOf("<tr>", trCloseIndex);
            trCloseIndex = html.indexOf("</tr>", trOpenIndex);
        }
    }

    void readBlueprints(String componentName, String modificationName, String url) throws Exception {
        String html = readUrl(url);
        Pattern pSection = Pattern.compile("<h3 (.+?)<br></div></div>");
        Matcher mSection = pSection.matcher(html);
        while (mSection.find()) {
            String section = mSection.group(1);

            String grade = "";
            Pattern pGrade = Pattern.compile(".*\\(Grade\\s+(\\d+)\\)</h3>.*");
            Matcher mGrade = pGrade.matcher(section);
            if (mGrade.matches()) {
                grade = mGrade.group(1);
            }

            // 1: Count
            // 2: Name
            Map<InaraItem, Integer> ingredients = new LinkedHashMap<>();
            Pattern pCost = Pattern.compile(">(\\d)x &nbsp;.+?\"/galaxy-component/\\d+\".*?>(.+?)</a>");
            Matcher mCost = pCost.matcher(section);
            while (mCost.find()) {
                String name = mCost.group(2);
                String count = mCost.group(1);
                InaraItem item = this.knownItems.get(name);
                if (item == null) {
                    throw new RuntimeException("Unknown item '" + name + "' for grade " + grade + ": " + url);
                }
                ingredients.put(item, Integer.valueOf(count));
            }

            List<InaraEngineer> engineers = new ArrayList<>();
            Pattern pEngineers = Pattern.compile("<a href=\"/galaxy-engineer/\\d+\".+?>(.+?)</a>");
            Matcher mEngineers = pEngineers.matcher(section);
            while (mEngineers.find()) {
                String name = mEngineers.group(1).trim();
                InaraEngineer engineer = this.knownEngineers.get(name);
                if (engineer == null) {
                    throw new RuntimeException("Unknown engineer '" + name + "' for grade " + grade + ": " + url);
                }
                engineers.add(engineer);
            }

            InaraComponent component = new InaraComponent(componentName);
            InaraModification modification = new InaraModification(modificationName);
            this.knownBlueprints.add(new InaraBlueprint(component, modification, Integer.valueOf(grade), ingredients, engineers));
        }
    }

    String readUrl(String url) throws Exception {
        HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
        InputStreamReader reader = new InputStreamReader(new BufferedInputStream(conn.getInputStream()), "UTF-8");
        StringWriter writer = new StringWriter();
        IOUtils.copy(reader, writer);
        IOUtils.closeQuietly(reader);
        conn.disconnect();
        return writer.toString();
    }

    public static class InaraComponent {

        private String name = "";

        public InaraComponent(String name) {
            this.name = name;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            InaraComponent other = (InaraComponent) obj;
            if (this.name == null) {
                if (other.name != null) {
                    return false;
                }
            } else if (!this.name.equals(other.name)) {
                return false;
            }
            return true;
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name;
        }

    }

    public static class InaraModification {

        private String name = "";

        public InaraModification(String name) {
            this.name = name;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            InaraModification other = (InaraModification) obj;
            if (this.name == null) {
                if (other.name != null) {
                    return false;
                }
            } else if (!this.name.equals(other.name)) {
                return false;
            }
            return true;
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name;
        }

    }

    public static class InaraItem {

        private String name = "";
        private String type = "";
        private int grade = 0;

        public InaraItem(String name, String type, int grade) {
            this.name = name;
            this.type = type;
            this.grade = grade;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            InaraItem other = (InaraItem) obj;
            if (this.name == null) {
                if (other.name != null) {
                    return false;
                }
            } else if (!this.name.equals(other.name)) {
                return false;
            }
            return true;
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getType() {
            return this.type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public int getGrade() {
            return this.grade;
        }

        public void setGrade(int grade) {
            this.grade = grade;
        }

    }

    public static class InaraEngineer {

        private String name = "";
        private String system = "";
        private String base = "";

        public InaraEngineer(String name, String system, String base) {
            this.name = name;
            this.system = system;
            this.base = base;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            InaraEngineer other = (InaraEngineer) obj;
            if (this.name == null) {
                if (other.name != null) {
                    return false;
                }
            } else if (!this.name.equals(other.name)) {
                return false;
            }
            return true;
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getSystem() {
            return this.system;
        }

        public void setSystem(String system) {
            this.system = system;
        }

        public String getBase() {
            return this.base;
        }

        public void setBase(String base) {
            this.base = base;
        }

    }

    public static class InaraBlueprint {

        private InaraComponent component = null;
        private InaraModification modification = null;
        private int grade = 0;
        private Map<InaraItem, Integer> ingredients = new LinkedHashMap<>();
        private List<InaraEngineer> engineers = new ArrayList<>();

        public InaraBlueprint(InaraComponent component, InaraModification modification, int grade, Map<InaraItem, Integer> ingredients, List<InaraEngineer> engineers) {
            this.component = component;
            this.modification = modification;
            this.grade = grade;
            this.ingredients = ingredients;
            this.engineers = engineers;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.component == null) ? 0 : this.component.hashCode());
            result = prime * result + this.grade;
            result = prime * result + ((this.modification == null) ? 0 : this.modification.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            InaraBlueprint other = (InaraBlueprint) obj;
            if (this.component == null) {
                if (other.component != null) {
                    return false;
                }
            } else if (!this.component.equals(other.component)) {
                return false;
            }
            if (this.grade != other.grade) {
                return false;
            }
            if (this.modification == null) {
                if (other.modification != null) {
                    return false;
                }
            } else if (!this.modification.equals(other.modification)) {
                return false;
            }
            return true;
        }

        public InaraComponent getComponent() {
            return this.component;
        }

        public void setComponent(InaraComponent component) {
            this.component = component;
        }

        public InaraModification getModification() {
            return this.modification;
        }

        public void setModification(InaraModification modification) {
            this.modification = modification;
        }

        public int getGrade() {
            return this.grade;
        }

        public void setGrade(int grade) {
            this.grade = grade;
        }

        public Map<InaraItem, Integer> getIngredients() {
            return this.ingredients;
        }

        public void setIngredients(Map<InaraItem, Integer> ingredients) {
            this.ingredients = ingredients;
        }

        public List<InaraEngineer> getEngineers() {
            return this.engineers;
        }

        public void setEngineers(List<InaraEngineer> engineers) {
            this.engineers = engineers;
        }

    }

}
