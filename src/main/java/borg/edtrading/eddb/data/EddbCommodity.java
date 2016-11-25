package borg.edtrading.eddb.data;

import com.google.gson.annotations.SerializedName;

/**
 * EddbCommodity
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbCommodity implements EddbEntity {

    private static final long serialVersionUID = 5729070143960340119L;

    @SerializedName("id")
    private Long id = null;
    @SerializedName("name")
    private String name = null;
    @SerializedName("average_price")
    private Integer averagePrice = null;
    @SerializedName("is_rare")
    private Boolean isRare = null;
    @SerializedName("category_id")
    private Long categoryId = null;
    @SerializedName("category")
    private Category category = null;

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
        EddbCommodity other = (EddbCommodity) obj;
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
        this.name = name;
    }

    public Integer getAveragePrice() {
        return this.averagePrice;
    }

    public void setAveragePrice(Integer averagePrice) {
        this.averagePrice = averagePrice;
    }

    public Boolean getIsRare() {
        return this.isRare;
    }

    public void setIsRare(Boolean isRare) {
        this.isRare = isRare;
    }

    public Long getCategoryId() {
        return this.categoryId;
    }

    public void setCategoryId(Long categoryId) {
        this.categoryId = categoryId;
    }

    public Category getCategory() {
        return this.category;
    }

    public void setCategory(Category category) {
        this.category = category;
    }

    public static class Category implements EddbEntity {

        private static final long serialVersionUID = -8865236596300999299L;

        @SerializedName("id")
        private Long id = null;
        @SerializedName("name")
        private String name = null;

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
            Category other = (Category) obj;
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

    }

}
