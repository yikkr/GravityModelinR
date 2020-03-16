# Survey at angler shops
### Set number of angler shops to survey
drops <- c("OBJECTID", "HUC_8")
angler_shops_table<-example_table[ , (names(example_table) %in% drops)]
angler_shops_table$shops_num <- 0
# from google map
angler_shops_table$shops_num <- 0
angler_shops_table[angler_shops_table$HUC_8 == "04020801", "shops_num"] <- 2
angler_shops_table[angler_shops_table$HUC_8 == "08010301", "shops_num"] <- 2
angler_shops_table[angler_shops_table$HUC_8 == "11020201", "shops_num"] <- 2
angler_shops_table[angler_shops_table$HUC_8 == "08010302", "shops_num"] <- 2
angler_shops_table[angler_shops_table$HUC_8 == "04020803", "shops_num"] <- 2
angler_shops_table[angler_shops_table$HUC_8 == "04020401", "shops_num"] <- 2
angler_shops_table[angler_shops_table$HUC_8 == "04021001", "shops_num"] <- 3
angler_shops_table[angler_shops_table$HUC_8 == "11030101", "shops_num"] <- 5
## save the frame first
write.csv(angler_shops_table, "angler_shops_table.csv")
## Read here
angler_shops_table_1<-read.csv("angler_shops_table.csv")
## Remember to deal with the first column
###
personhours <- 200
angler_shops_table$shops_num <- angler_shops_table$shops_num*personhours
score_shop_table <- angler_shops_table
score_shop_table$total_score <- 0
## distance parameter
c_d <- 0.1e-3
for (row in 1:nrow(angler_shops_table)) {
  current_huc <- score_shop_table[row,"OBJECTID"]
  score_sum <- 0
  for (huc in 1:nrow(score_shop_table)) {
    tempt_huc <- score_shop_table[huc,"OBJECTID"]
    if (tempt_huc == current_huc) {
      d_ij <- 0
    } else if (tempt_huc < current_huc) {
      d_ij <- subset(example77_distance, INPUT_FID==tempt_huc & NEAR_FID==current_huc)$DISTANCE
    } else if (tempt_huc > current_huc) {
      d_ij <- subset(example77_distance, INPUT_FID==current_huc & NEAR_FID==tempt_huc)$DISTANCE
    }
    eff <- score_shop_table[huc,"shops_num"]
    score_sum <- score_sum+ eff/exp(c_d*d_ij)
    d_ij <- NA ## for checking
  }
  score_shop_table[row,"total_score"] <- score_sum
}
score_shop_table["total_score"] <- score_shop_table["total_score"]/sum(score_shop_table["total_score"])
