.aaf.envbuild <- function() {

    subenv <- function (oldenv, names) {
    
        newenv <- new.env(TRUE, NULL);
        for (name in names) {
            value <- do.call("get", list(name, as.name(oldenv), inherits=F))
            do.call("assign", list(name, value, newenv))
        }
    
        return(newenv);
    }
    
    saveannaffy <- function (probeids, chip, type) {
    
        oldenvname <- paste(chip, type, sep="")
        newenvname <- paste("annaffy", type, sep = "")
        do.call("assign", list(newenvname, subenv(oldenvname, probeids)))
        do.call("save", list(newenvname, file = paste(newenvname, ".rda", sep = "")))
    }
    
    require(hgu95av2)
    
    probeids <- c("32547_at", "720_at", "40142_at", "38903_at", "34834_at", 
                  "37049_g_at", "2021_s_at", "38375_at", "41770_at",
                  "32354_at", "35256_at", "33201_at", "35214_at", "36387_at",
                  "40685_at", "1974_s_at", "32815_at", "39496_s_at",
                  "40806_at", "570_at", "35205_at", "34111_s_at", "34262_at",
                  "36912_at", "39001_at", "38412_at", "40160_at", "33603_at",
                  "40864_at", "41178_at", "37644_s_at", "40944_at",
                  "31693_f_at", "39440_f_at", "1099_s_at", "41157_at",
                  "417_at", "33742_f_at", "39600_at", "234_s_at", "35174_i_at",
                  "33511_at", "36097_at", "831_at", "131_at", "31537_at",
                  "37031_at", "34594_at", "38324_at", "34200_at", "40372_at",
                  "37238_s_at", "41394_at", "40112_at", "39687_at", "36640_at",
                  "768_at", "31654_at", "41316_s_at", "556_s_at", "31901_at",
                  "33759_at", "32954_at", "34745_at", "39651_at", "40651_s_at",
                  "33332_at", "35315_at", "31848_at", "35728_at", "1435_f_at",
                  "36662_at", "35168_f_at", "36962_at", "32733_at", "1027_at",
                  "198_g_at", "33725_at", "41095_at", "34826_at", "584_s_at",
                  "41018_at", "163_at", "36519_at", "154_at", "37307_at",
                  "1144_at", "34364_at", "40614_at", "32443_at", "33048_at",
                  "37628_at", "34195_at", "35441_at", "32988_at", "40357_at",
                  "33114_f_at", "1165_at", "35612_at", "37822_at", "33098_at",
                  "40675_r_at", "39501_f_at", "35492_at", "36848_r_at",
                  "36379_at", "34051_at", "1214_s_at", "34920_at", "262_at",
                  "35933_f_at", "40218_at", "33715_r_at", "36267_at",
                  "33012_at", "1495_at", "1221_at", "35815_at", "31622_f_at",
                  "41161_at", "31521_f_at", "33573_at", "40186_at", "34815_at",
                  "33973_at", "38283_at", "32692_at", "32498_at", "35353_at",
                  "40217_s_at", "33186_i_at", "37808_at", "39997_at",
                  "38225_at", "32230_at", "652_g_at", "38981_at", "31469_s_at",
                  "2090_i_at", "39722_at", "35857_at", "36508_at", "39271_at",
                  "36593_at", "38481_at", "908_at", "41531_at",
                  "AFFX-HUMGAPDH/M33197_M_at", "35494_at", "35854_at",
                  "37279_at", "40125_at", "35154_at", "34438_at", "36238_at",
                  "1899_s_at", "39073_at", "36568_at", "39619_at", "256_s_at",
                  "33806_at", "40563_at", "31687_f_at", "31552_at", "39919_at",
                  "37598_at", "500_at", "1914_at", "35596_at", "610_at",
                  "32073_at", "34840_at", "209_at", "40990_at", "35209_at",
                  "39350_at", "40873_at", "39005_s_at", "39421_at", "1470_at",
                  "37799_at", "32544_s_at", "37903_at", "35087_at", "151_s_at",
                  "41757_at", "38318_at", "40771_at", "36116_at", "880_at",
                  "508_at", "32879_at", "40401_at", "35488_at", "1096_g_at",
                  "33884_s_at", "36113_s_at", "32010_at", "40600_at",
                  "41205_at", "40323_at", "35574_i_at", "36790_at", "34695_at",
                  "37572_at", "37338_at", "40466_at", "914_g_at", "35977_at",
                  "971_s_at", "1986_at", "1724_at", "40016_g_at", "35447_s_at",
                  "32701_at", "38587_at", "1487_at", "37830_at", "33251_at",
                  "702_f_at", "37734_at", "38023_at", "40605_at", "37602_at",
                  "38018_g_at", "34597_at", "31504_at", "38413_at", "1506_at",
                  "1676_s_at", "40336_at", "1088_at", "37942_at", "41329_at",
                  "41237_at", "36156_at", "36499_at", "33145_at", "35858_at",
                  "33304_at", "32525_r_at", "32155_at", "32768_at", "40189_at",
                  "35772_at", "38207_at", "33720_at", "38573_at", "38149_at",
                  "1065_at")

        
    types <- c("ACCNUM", "CHRLOC", "CHR", "ENZYME", "GENENAME", "GO", "LOCUSID", 
               "MAP", "PATH", "PMID", "SUMFUNC", "SYMBOL", "UNIGENE")
    
    for(type in types)
        saveannaffy(probeids, "hgu95av2", type)
}
