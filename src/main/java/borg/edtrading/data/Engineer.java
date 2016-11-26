package borg.edtrading.data;

/**
 * Engineer
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum Engineer {

    //@formatter:off
    BILL_TURNER                                       ("Bill Turner"                                     , ""                                                , ""),
    BROO_TARQUIN                                      ("Broo Tarquin"                                    , "Muang"                                           , "Broo's Legacy"),
    COLONEL_BRIS_DEKKER                               ("Colonel Bris Dekker"                             , ""                                                , ""),
    DIDI_VATERMANN                                    ("Didi Vatermann"                                  , "Leesti"                                          , "Vatermann LLC"),
    ELVIRA_MARTUUK                                    ("Elvira Martuuk"                                  , "Khun"                                            , "Long Sight Base"),
    FELICITY_FARSEER                                  ("Felicity Farseer"                                , "Deciat"                                          , "Farseer Inc"),
    HERA_TANI                                         ("Hera Tani"                                       , "Kuwemaki"                                        , "The Jet's Hole"),
    JURI_ISHMAAK                                      ("Juri Ishmaak"                                    , "Giryak"                                          , "Pater's Memorial"),
    LEI_CHEUNG                                        ("Lei Cheung"                                      , "Laksak"                                          , "Trader's Rest"),
    LIZ_RYDER                                         ("Liz Ryder"                                       , "Eurybia"                                         , "Demolition Unlimited"),
    LORI_JAMESON                                      ("Lori Jameson"                                    , "Shinrarta Dezhra"                                , "Jameson Base"),
    MARCO_QWENT                                       ("Marco Qwent"                                     , "Sirius"                                          , "Qwent Research Base"),
    PROFESSOR_PALIN                                   ("Professor Palin"                                 , "Maia"                                            , "Palin Research Centre"),
    RAM_TAH                                           ("Ram Tah"                                         , "Meene"                                           , "Phoenix Base"),
    SELENE_JEAN                                       ("Selene Jean"                                     , "Kuk"                                             , "Prospector's Rest"),
    THE_DWELLER                                       ("The Dweller"                                     , "Wyrd"                                            , "Black Hide"),
    THE_SARGE                                         ("The Sarge"                                       , "Beta-3 Tucani"                                   , "The Beach"),
    TIANA_FORTUNE                                     ("Tiana Fortune"                                   , "Achenar"                                         , "Fortune's Loss"),
    TOD_THE_BLASTER_MCQUINN                           ("Tod \"The Blaster\" McQuinn"                     , "Wolf 397"                                        , "Trophy Camp"),
    ZACARIAH_NEMO                                     ("Zacariah Nemo"                                   , "Yoru"                                            , "Nemo Cyber Party Base");
    //@formatter:on

    private final String name;
    private final String system;
    private final String base;

    private Engineer(String name, String system, String base) {
        this.name = name;
        this.system = system;
        this.base = base;
    }

    public String getName() {
        return this.name;
    }

    public String getSystem() {
        return this.system;
    }

    public String getBase() {
        return this.base;
    }

}
