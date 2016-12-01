package borg.edtrading.eddb.data;

import com.google.gson.annotations.SerializedName;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;

/**
 * EddbModule
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
@Document(indexName = "eddbmodule", type = "eddbmodule", shards = 1, replicas = 0)
public class EddbModule implements EddbEntity {

    private static final long serialVersionUID = 6837140492504224340L;

    @Id
    @SerializedName("id")
    private Long id = null;
    @SerializedName("name")
    private String name = null; // Only set for powerplay modules
    @SerializedName("class")
    private Integer size = null;
    @SerializedName("rating")
    private String rating = null;
    @SerializedName("price")
    private Integer price = null;
    @SerializedName("weapon_mode")
    private String weaponMode = null; // Fixed etc
    @SerializedName("missile_type")
    private Integer missileType = null; // ?
    @SerializedName("belongs_to")
    private Long belongsTo = null; // ?
    //    @SerializedName("ed_id")
    //    private Long edId = null;
    //    @SerializedName("ed_symbol")
    //    private String edSymbol = null;
    @SerializedName("ship")
    private String ship = null;
    @SerializedName("group_id")
    private Long groupId = null;
    @SerializedName("group")
    private EddbModuleGroup group = null;

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (this.getClass() != obj.getClass()) {
            return false;
        }
        EddbModule other = (EddbModule) obj;
        if (this.id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!this.id.equals(other.id)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return this.id.hashCode();
    }

    @Override
    public String toString() {
        return "#" + this.id;
    }

    @Override
    public Long getId() {
        return this.id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getSize() {
        return this.size;
    }

    public void setSize(Integer size) {
        this.size = size;
    }

    public String getRating() {
        return this.rating;
    }

    public void setRating(String rating) {
        this.rating = rating == null ? null : rating.intern();
    }

    public Integer getPrice() {
        return this.price;
    }

    public void setPrice(Integer price) {
        this.price = price;
    }

    public String getWeaponMode() {
        return this.weaponMode;
    }

    public void setWeaponMode(String weaponMode) {
        this.weaponMode = weaponMode == null ? null : weaponMode.intern();
    }

    public Integer getMissileType() {
        return this.missileType;
    }

    public void setMissileType(Integer missileType) {
        this.missileType = missileType;
    }

    public Long getBelongsTo() {
        return this.belongsTo;
    }

    public void setBelongsTo(Long belongsTo) {
        this.belongsTo = belongsTo;
    }

    //    public Long getEdId() {
    //        return this.edId;
    //    }
    //
    //    public void setEdId(Long edId) {
    //        this.edId = edId;
    //    }
    //
    //    public String getEdSymbol() {
    //        return this.edSymbol;
    //    }
    //
    //    public void setEdSymbol(String edSymbol) {
    //        this.edSymbol = edSymbol;
    //    }

    public String getShip() {
        return this.ship;
    }

    public void setShip(String ship) {
        this.ship = ship == null ? null : ship.intern();
    }

    public Long getGroupId() {
        return this.groupId;
    }

    public void setGroupId(Long groupId) {
        this.groupId = groupId;
    }

    public EddbModuleGroup getGroup() {
        return this.group;
    }

    public void setGroup(EddbModuleGroup group) {
        this.group = group;
    }

    public static class EddbModuleGroup implements EddbEntity {

        private static final long serialVersionUID = 7405734738160782320L;

        @SerializedName("id")
        private Long id = null;
        @SerializedName("name")
        private String name = null;
        @SerializedName("category_id")
        private Long categoryId = null;
        @SerializedName("category")
        private String category = null;

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (this.getClass() != obj.getClass()) {
                return false;
            }
            EddbModuleGroup other = (EddbModuleGroup) obj;
            if (this.id == null) {
                if (other.id != null) {
                    return false;
                }
            } else if (!this.id.equals(other.id)) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            return this.id.hashCode();
        }

        @Override
        public String toString() {
            return "#" + this.id + " " + this.name;
        }

        @Override
        public Long getId() {
            return this.id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name == null ? null : name.intern();
        }

        public Long getCategoryId() {
            return this.categoryId;
        }

        public void setCategoryId(Long categoryId) {
            this.categoryId = categoryId;
        }

        public String getCategory() {
            return this.category;
        }

        public void setCategory(String category) {
            this.category = category == null ? null : category.intern();
        }

    }

}
