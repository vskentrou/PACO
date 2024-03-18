#### INSTALL PACKAGES ####
install.packages(c("TripleR", "srm", "lme4", "nlme", "plyr", "dplyr", "mlma",
                   "r2mlm", "lmerTest", "performance", "MuMIn", "truncnorm"))
install.packages("lavaan", dependencies = TRUE)
remotes::install_github("TDJorgensen/lavaan.srm")

# load packages
my_packages <- c("TripleR", "srm", "lme4", "nlme", "plyr", "dplyr", "mlma",
                 "r2mlm", "lmerTest", "performance", "MuMIn", "lavaan", "truncnorm")
lapply(my_packages, library, character.only = TRUE)
(.packages())

library(dplyr)
library(srm)

#### IMPORT THE DATA ####
setwd("~/Desktop/3 PACO Project/3 PACO Data/0 Latest PACO Data")

# this is the RAW data
intake_session <- read.csv("intake_session.csv") #intake session
conversation_session <- read.csv("conv_data.csv") #conversation session
cooperation_session <- read.csv("coop_data.csv") #cooperation session
collaboration_session <- read.csv("coll_data.csv") #collaboration session
impression_session <- read.csv("imp_data.csv") #impression session

# this is the CLEANED data
# these files contain the HEXACO SRM effects for the conversation and collaboration sessions
ind_conv <- read.csv("tar_per_conv.csv")
rel_conv <- read.csv("rel_conv.csv")
ind_coll <- read.csv("tar_per_coll.csv")
rel_coll <- read.csv("rel_coll.csv")


##### CLEAN/PREPARE DATA FOR SRAs #####

# clean the data so that perceiver, target, and group IDs are suitable for SRAs
# remove letters in front of the group, perceiver, and target ID variables

# impression phase
impression_session$imp_selfLPID <- as.character(impression_session$imp_selfLPID)
impression_session$imp_selfLPID <- gsub("^.{0,5}", "", impression_session$imp_selfLPID) # Replace first 5 characters with empty string ""

impression_session$imp_otherLPID <- as.character(impression_session$imp_otherLPID)
impression_session$imp_otherLPID <- gsub("^.{0,5}", "", impression_session$imp_otherLPID) # Replace first 5 characters with empty string ""

impression_session$batchID <- as.character(impression_session$batchID)
impression_session$batchID <- gsub("^.{0,6}", "", impression_session$batchID) # Replace first 5 characters with empty string ""

# conversation phase
conversation_session$conv_selfLPID <- as.character(conversation_session$conv_selfLPID)
conversation_session$conv_selfLPID <- gsub("^.{0,5}", "", conversation_session$conv_selfLPID) # Replace first 5 characters with empty string ""

conversation_session$conv_otherLPID <- as.character(conversation_session$conv_otherLPID)
conversation_session$conv_otherLPID <- gsub("^.{0,5}", "", conversation_session$conv_otherLPID) # Replace first 5 characters with empty string ""

conversation_session$batchID <- as.character(conversation_session$batchID)
conversation_session$batchID <- gsub("^.{0,6}", "", conversation_session$batchID) # Replace first 6 characters with empty string ""

# cooperation phase
cooperation_session$coord_selfLPID <- as.character(cooperation_session$coord_selfLPID)
cooperation_session$coord_selfLPID <- gsub("^.{0,5}", "", cooperation_session$coord_selfLPID) # Replace first 5 characters with empty string ""

cooperation_session$coord_otherLPID <- as.character(cooperation_session$coord_otherLPID)
cooperation_session$coord_otherLPID <- gsub("^.{0,5}", "", cooperation_session$coord_otherLPID) # Replace first 5 characters with empty string ""

cooperation_session$batchID <- as.character(cooperation_session$batchID)
cooperation_session$batchID <- gsub("^.{0,6}", "", cooperation_session$batchID) # Replace first 6 characters with empty string ""

# collaboration phase
collaboration_session$coll_selfLPID <- as.character(collaboration_session$coll_selfLPID)
collaboration_session$coll_selfLPID <- gsub("^.{0,5}", "", collaboration_session$coll_selfLPID)

collaboration_session$coll_otherLPID <- as.character(collaboration_session$coll_otherLPID)
collaboration_session$coll_otherLPID <- gsub("^.{0,5}", "", collaboration_session$coll_otherLPID)

collaboration_session$batchID <- as.character(collaboration_session$batchID)
collaboration_session$batchID <- gsub("^.{0,6}", "", collaboration_session$batchID)

# rename relevant columns to "perceiver.id', "target.id", and "group.id"

# impression phase
names(impression_session)[names(impression_session) == "imp_selfLPID"] <- "perceiver.id"
names(impression_session)[names(impression_session) == "imp_otherLPID"] <- "target.id"
names(impression_session)[names(impression_session) == "batchID"] <- "group.id"

# conversation phase
names(conversation_session)[names(conversation_session) == "conv_selfLPID"] <- "perceiver.id"
names(conversation_session)[names(conversation_session) == "conv_otherLPID"] <- "target.id"
names(conversation_session)[names(conversation_session) == "batchID"] <- "group.id"

# cooperation phase
names(cooperation_session)[names(cooperation_session) == "coord_selfLPID"] <- "perceiver.id"
names(cooperation_session)[names(cooperation_session) == "coord_otherLPID"] <- "target.id"
names(cooperation_session)[names(cooperation_session) == "batchID"] <- "group.id"

# collaboration phase
names(collaboration_session)[names(collaboration_session) == "coll_selfLPID"] <- "perceiver.id"
names(collaboration_session)[names(collaboration_session) == "coll_otherLPID"] <- "target.id"
names(collaboration_session)[names(collaboration_session) == "batchID"] <- "group.id"

# convert vectors of character values into numeric values

impression_session$perceiver.id <- as.numeric(as.character(impression_session$perceiver.id))
conversation_session$perceiver.id <- as.numeric(as.character(conversation_session$perceiver.id))
cooperation_session$perceiver.id <- as.numeric(as.character(cooperation_session$perceiver.id))
collaboration_session$perceiver.id <- as.numeric(as.character(collaboration_session$perceiver.id))

impression_session$target.id <- as.numeric(as.character(impression_session$target.id))
conversation_session$target.id <- as.numeric(as.character(conversation_session$target.id))
cooperation_session$target.id <- as.numeric(as.character(cooperation_session$target.id))
collaboration_session$target.id <- as.numeric(as.character(collaboration_session$target.id))

impression_session$group.id <- as.numeric(as.character(impression_session$group.id))
conversation_session$group.id <- as.numeric(as.character(conversation_session$group.id))
cooperation_session$group.id <- as.numeric(as.character(cooperation_session$group.id))
collaboration_session$group.id <- as.numeric(as.character(collaboration_session$group.id))



##### SRAs: HEXACO #####

# in the SRM analyses below, a single indicator is used for each construct

library(TripleR)
RR.style("perception") # the terms "perceiver" and "target" will be used in the output

## CONVERSATION PHASE: HEXACO SRAs

R1 <- RR(conv_PP_Hex_HH ~ perceiver.id * target.id | group.id, data = conversation_session, na.rm = TRUE) # honesty-humility

R2 <- RR(conv_PP_Hex_HE ~ perceiver.id * target.id | group.id, data = conversation_session, na.rm = TRUE) # emotionality

R3 <- RR(conv_PP_Hex_HX ~ perceiver.id * target.id | group.id, data = conversation_session, na.rm = TRUE) # extraversion

R4 <- RR(conv_PP_Hex_HA ~ perceiver.id * target.id | group.id, data = conversation_session, na.rm = TRUE) # agreeableness

R5 <- RR(conv_PP_Hex_HC ~ perceiver.id * target.id | group.id, data = conversation_session, na.rm = TRUE) # conscientiousness

R6 <- RR(conv_PP_Hex_HO ~ perceiver.id * target.id | group.id, data = conversation_session, na.rm = TRUE) # openness to experience


## COLLABORATION PHASE: HEXACO SRAs

# first remove a duplicate row in the data that prevents the SRM analysis from running
# (check if this could be a data entry issue, duplicate row number in the original dataset is 183)
collaboration_session <- collaboration_session[-c(183:183), ]

R7 <- RR(coll_PP_Hex_HH ~ perceiver.id * target.id | group.id, data = collaboration_session, na.rm = TRUE) # honesty-humility

R8 <- RR(coll_PP_Hex_HE ~ perceiver.id * target.id | group.id, data = collaboration_session, na.rm = TRUE) # emotionality

R9 <- RR(coll_PP_Hex_HX ~ perceiver.id * target.id | group.id, data = collaboration_session, na.rm = TRUE) # extraversion

R10 <- RR(coll_PP_Hex_HA ~ perceiver.id * target.id | group.id, data = collaboration_session, na.rm = TRUE) # agreeableness

R11 <- RR(coll_PP_Hex_HC ~ perceiver.id * target.id | group.id, data = collaboration_session, na.rm = TRUE) # conscientiousness

R12 <- RR(coll_PP_Hex_HO ~ perceiver.id * target.id | group.id, data = collaboration_session, na.rm = TRUE) # openness to experience

# save the target and perceiver effects into new data frames

srm_effects1 <- R1$effects # honesty-humility - conversation phase
srm_effects2 <- R2$effects # emotionality - conversation phase
srm_effects3 <- R3$effects # extraversion - conversation phase
srm_effects4 <- R4$effects # agreeableness - conversation phase
srm_effects5 <- R5$effects # conscientiousness - conversation phase
srm_effects6 <- R6$effects # openness to experience - conversation phase

srm_effects7 <- R7$effects # honesty-humility - collaboration phase
srm_effects8 <- R8$effects # emotionality - collaboration phase
srm_effects9 <- R9$effects # extraversion - collaboration phase
srm_effects10 <- R10$effects # agreeableness - collaboration phase
srm_effects11 <- R11$effects # conscientiousness - collaboration phase
srm_effects12 <- R12$effects # openness to experience - collaboration phase


# connect the unique participant IDs to their corresponding SRM effects

# conversation phase SRM effects

global_ids_conv <- gsub("^.{0,4}", "", conversation_session$globalPID[order(conversation_session$group.id)]) # replace first 4 characters with empty string ""
global_ids_conv <- as.numeric(as.character(global_ids_conv)) #convert vector of character values into numeric values
ids_conv <- global_ids_conv[!duplicated(global_ids_conv)] # remove "duplicates" from the participant ID column (duplicates are due to the data format required for SRM analyses)

# add "id" columns to each data frame to indicate the unique participant ID
effects_conv <- list(srm_effects1, srm_effects2, srm_effects3, srm_effects4,
                     srm_effects5, srm_effects6)
for (i in 1:length(effects_conv)) {
  effects_conv[[i]]$id <- ids_conv }

# collaboration phase SRM effects

global_ids_coll <- gsub("^.{0,4}", "", collaboration_session$globalPID[order(collaboration_session$group.id)]) # replace first 4 characters with empty string ""
global_ids_coll <- as.numeric(as.character(global_ids_coll)) #convert vector of character values into numeric values
ids_coll <- global_ids_coll[!duplicated(global_ids_coll)] # remove "duplicates" from the participant ID column

# participant 618 (perceiver 2 in group 65) was removed by TripleR from the SRM analyses
# due to excessive missing data, so I have also removed them from the participant list:
ids_coll <- ids_coll[!ids_coll == "618"]

# add "id" columns to each data frame to indicate the unique participant ID
effects_coll <- list(srm_effects7, srm_effects8, srm_effects9, srm_effects10,
                     srm_effects11, srm_effects12)
for (i in 1:length(effects_coll)) {
  effects_coll[[i]]$id <- ids_coll }


# combine the SRM effects into one data frame

# conversation phase

effects_conversation_phase <- bind_cols(effects_conv, id = NULL) # combine all SRM effects into one data frame
effects_conversation_phase <- subset(effects_conversation_phase,
                                     select = -c(id...5, group.id...6,
                                                 id...9, group.id...10,
                                                 id...13, group.id...14,
                                                 id...17, group.id...18,
                                                 id...21, group.id...22)) # remove duplicate columns for id and group.id

names(effects_conversation_phase)[names(effects_conversation_phase) == 'id...1'] <- 'id'
names(effects_conversation_phase)[names(effects_conversation_phase) == 'group.id...2'] <- 'group.id'

effects_conversation_phase$group.id <- as.numeric(as.character(effects_conversation_phase$group.id))
write.csv(effects_conversation_phase, "tar_per_conv.csv")
# tar_per_conv <- read.csv("tar_per_conv.csv") # data file that contains the HEXACO target and perceiver effects for the conversation phase

# RELATIONSHIP EFFECTS

hh_rel <- R1$effectsRel
he_rel <- R2$effectsRel
hx_rel <- R3$effectsRel
ha_rel <- R4$effectsRel
hc_rel <- R5$effectsRel
ho_rel <- R6$effectsRel

rel_effects_conv <- list(hh_rel, he_rel, hx_rel, ha_rel, hc_rel, ho_rel)
rel_effects_conv <- bind_cols(rel_effects_conv, id = NULL) # combine all SRM effects into one data frame
rel_effects_conv <- subset(rel_effects_conv,
                           select = -c(group.id...6, perceiver.id...7,
                                       target.id...8, dyad...9, group.id...11,
                                       perceiver.id...12, target.id...13,
                                       dyad...14, group.id...16,
                                       perceiver.id...17, target.id...18,
                                       dyad...19, group.id...21,
                                       perceiver.id...22, target.id...23,
                                       dyad...24, group.id...26,
                                       perceiver.id...27, target.id...28,
                                       dyad...29)) # remove duplicate columns for id and group.id

colnames(rel_effects_conv) <- c("group.id","perceiver.id", "target.id", "dyad",
                                "rel_hh", "rel_he", "rel_hx", "rel_ha",
                                "rel_hc", "rel_ho")

rel_effects_conv$group.id <- as.numeric(as.character(rel_effects_conv$group.id))
rel_effects_conv$perceiver.id <- as.numeric(as.character(rel_effects_conv$perceiver.id))
rel_effects_conv$target.id <- as.numeric(as.character(rel_effects_conv$target.id))

write.csv(rel_effects_conv, "rel_conv.csv") # create a .csv file of the relationship effects from the conversation phase
rel_conv <- read.csv("rel_conv.csv") # data file that contains the HEXACO relationship effects for the conversation phase

# collaboration phase

effects_collaboration_phase <- bind_cols(effects_coll, id = NULL) # combine all SRM effects into one data frame
effects_collaboration_phase <- subset(effects_collaboration_phase,
                                      select = -c(id...5, group.id...6,
                                                  id...9, group.id...10,
                                                  id...13, group.id...14,
                                                  id...17, group.id...18,
                                                  id...21, group.id...22)) # remove duplicate columns for id and group.id


names(effects_collaboration_phase)[names(effects_collaboration_phase) == 'id...1'] <- 'id'
names(effects_collaboration_phase)[names(effects_collaboration_phase) == 'group.id...2'] <- 'group.id'

effects_collaboration_phase$group.id <- as.numeric(as.character(effects_collaboration_phase$group.id))
write.csv(effects_collaboration_phase, "tar_per_coll.csv")

# RELATIONSHIP EFFECTS

hh_rel_coll <- R7$effectsRel
he_rel_coll <- R8$effectsRel
hx_rel_coll <- R9$effectsRel
ha_rel_coll <- R10$effectsRel
hc_rel_coll <- R11$effectsRel
ho_rel_coll <- R12$effectsRel

rel_effects_coll <- list(hh_rel_coll, he_rel_coll, hx_rel_coll, ha_rel_coll,
                         hc_rel_coll, ho_rel_coll)
rel_effects_coll <- bind_cols(rel_effects_coll, id = NULL) # combine all SRM effects into one data frame
rel_effects_coll <- subset(rel_effects_coll,
                           select = -c(group.id...6, perceiver.id...7,
                                       target.id...8, dyad...9, group.id...11,
                                       perceiver.id...12, target.id...13,
                                       dyad...14, group.id...16,
                                       perceiver.id...17, target.id...18,
                                       dyad...19, group.id...21,
                                       perceiver.id...22, target.id...23,
                                       dyad...24, group.id...26,
                                       perceiver.id...27, target.id...28,
                                       dyad...29)) # remove duplicate columns for id and group.id

colnames(rel_effects_coll) <- c("group.id","perceiver.id", "target.id", "dyad",
                                "rel_hh_coll", "rel_he_coll", "rel_hx_coll",
                                "rel_ha_coll", "rel_hc_coll", "rel_ho_coll")

rel_effects_coll$group.id <- as.numeric(as.character(rel_effects_coll$group.id))
rel_effects_coll$perceiver.id <- as.numeric(as.character(rel_effects_coll$perceiver.id))
rel_effects_coll$target.id <- as.numeric(as.character(rel_effects_coll$target.id))

write.csv(rel_effects_coll, "rel_coll.csv") # create a .csv file of the relationship effects from the collaboration phase
rel_coll <- read.csv("rel_coll.csv") # data file that contains the HEXACO relationship effects for the collaboration phase

# combine the relationship effects from the two phases into one dataset

# first, match the relationship effects from the CONVERSATION phase to the unique participant IDs

rel_conv <- na.omit(rel_conv)
conversation_session$selfPID <- as.character(conversation_session$selfPID)
conversation_session$selfPID <- gsub("^.{0,4}", "", conversation_session$selfPID) # Replace first 5 characters with empty string ""

# merge relationship effects with global participant ID
merged_rel_conv <- merge(conversation_session, rel_conv,
                         by = c("group.id", "target.id", "perceiver.id"),
                         all.x = TRUE, all.y = TRUE)

# create a dataset that only contains columns of interest
rel_conv <- merged_rel_conv[c("group.id","target.id","perceiver.id",
                              "globalPID", "stageID", "file", "coll_conv_check",
                              "dyadID", "otherPID", "selfPID", "perspectiveID",
                              "dyad", "rel_hh", "rel_he", "rel_hx", "rel_ha",
                              "rel_hc", "rel_ho")]

# rename the "selfPID" column to "id"
#names(rel_conv)[names(rel_conv) == 'selfPID'] <- 'id'

write.csv(rel_conv, 'rel_conv.csv')

# next, match the relationship effects from the COLLABORATION phase to the unique participant IDs

rel_coll <- na.omit(rel_coll)
collaboration_session$selfPID <- as.character(collaboration_session$selfPID)
collaboration_session$selfPID <- gsub("^.{0,4}", "", collaboration_session$selfPID) # Replace first 5 characters with empty string ""

# merge relationship effects with global participant ID
merged_rel_coll <- merge(collaboration_session, rel_coll, by = c("group.id", "target.id", "perceiver.id"), all.x = TRUE, all.y = TRUE)

# create a dataset that only contains columns of interest
rel_coll <- merged_rel_coll[c("group.id","target.id","perceiver.id",
                              "globalPID", "stageID", "file", "dyadID",
                              "otherPID", "selfPID", "perspectiveID", "dyad",
                              "rel_hh_coll", "rel_he_coll", "rel_hx_coll",
                              "rel_ha_coll", "rel_hc_coll", "rel_ho_coll")]

# rename the "selfPID" column to "id"
# names(rel_coll)[names(rel_coll) == 'selfPID'] <- 'id'
write.csv(rel_coll, 'rel_coll.csv')

#### Create datasets with HEXACO SRA results ####

# HEXACO target and perceiver effects
hexaco_ind <- merge(effects_conversation_phase, effects_collaboration_phase,
                    by = c("id", "group.id"), all.x = TRUE, all.y = TRUE)
write.csv(hexaco_ind, "hexaco_ind.csv")

# HEXACO relationship effects
hexaco_rel <- merge(rel_conv, rel_coll, by = c("dyadID", "globalPID"), all.x = TRUE, all.y = TRUE)


# create a dataset that only contains columns of interest
hexaco_rel <- hexaco_rel[c("dyadID", "globalPID","group.id.x","target.id.x","perceiver.id.x",
                           "otherPID.x", "selfPID.x", "perspectiveID.x",
                           "dyad.x", "rel_hh", "rel_he", "rel_hx", "rel_ha", "rel_hc",
                           "rel_ho", "rel_hh_coll", "rel_he_coll", "rel_hx_coll",
                           "rel_ha_coll", "rel_hc_coll", "rel_ho_coll")]

names(hexaco_rel)[names(hexaco_rel) == "group.id.x"] <- "group.id"
names(hexaco_rel)[names(hexaco_rel) == "target.id.x"] <- "target.id"
names(hexaco_rel)[names(hexaco_rel) == "perceiver.id.x"] <- "perceiver.id"
names(hexaco_rel)[names(hexaco_rel) == "otherPID.x"] <- "otherPID"
names(hexaco_rel)[names(hexaco_rel) == "selfPID.x"] <- "selfPID"
names(hexaco_rel)[names(hexaco_rel) == "perspectiveID.x"] <- "perspectiveID"
names(hexaco_rel)[names(hexaco_rel) == "dyad.x"] <- "dyad"

write.csv(hexaco_rel, "hexaco_rel.csv")


#### SRAs: HEXACO HH & HX (USING TWO INDICATORS) ####

# honesty-humility

# conversation phase
### conv_PP_Hex_HH: honesty-humility
### conv_PP_honM2: honest
### conv_PP_sincM3: sincere

R_hh_conv_1 <- RR(conv_PP_Hex_HH/conv_PP_honM2 ~ perceiver.id * target.id | group.id,
                  data = conversation_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # hexaco hh & honesty

R_hh_conv_2 <- RR(conv_PP_Hex_HH/conv_PP_sincM3 ~ perceiver.id * target.id | group.id,
                  data = conversation_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # hexaco hh & sincerity

R_hh_conv_3 <- RR(conv_PP_honM2/conv_PP_sincM3 ~ perceiver.id * target.id | group.id,
                  data = conversation_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # honesty & sincerity


# collaboration phase
### coll_PP_Hex_HH: honesty-humility
### coll_PP_honM2: honest
### coll_PP_sincM3: sincere

R_hh_coll_1 <- RR(coll_PP_Hex_HH/coll_PP_honM2 ~ perceiver.id * target.id | group.id,
                  data = collaboration_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # hexaco hh & honesty

R_hh_coll_2 <- RR(coll_PP_Hex_HH/coll_PP_sincM3 ~ perceiver.id * target.id | group.id,
                  data = collaboration_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # hexaco hh & sincerity

R_hh_coll_3 <- RR(coll_PP_honM2/coll_PP_sincM3 ~ perceiver.id * target.id | group.id,
                  data = collaboration_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # honesty & sincerity


# extraversion

# conversation phase
### conv_PP_Hex_HX: extraversion
### conv_PP_socS2: sociable
### conv_PP_dom: dominant

R_hx_conv_1 <- RR(conv_PP_Hex_HX/conv_PP_socS2 ~ perceiver.id * target.id | group.id,
                  data = conversation_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # hexaco x & sociable

R_hx_conv_2 <- RR(conv_PP_Hex_HX/conv_PP_dom ~ perceiver.id * target.id | group.id,
                  data = conversation_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # hexaco x & dominant

R_hx_conv_3 <- RR(conv_PP_socS2/conv_PP_dom ~ perceiver.id * target.id | group.id,
                  data = conversation_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # sociable & dominant


# collaboration phase
### coll_PP_Hex_HX: extraversion
### coll_PP_socS2: sociable
### coll_PP_dom: dominant

R_hx_coll_1 <- RR(coll_PP_Hex_HX/coll_PP_socS2 ~ perceiver.id * target.id | group.id,
                  data = collaboration_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # hexaco x & sociable

R_hx_coll_2 <- RR(coll_PP_Hex_HH/coll_PP_sincM3 ~ perceiver.id * target.id | group.id,
                  data = collaboration_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # hexaco x & dominant

R_hx_coll_3 <- RR(coll_PP_honM2/coll_PP_sincM3 ~ perceiver.id * target.id | group.id,
                  data = collaboration_session, na.rm = TRUE, varname="honesty-humility",
                  se="LashleyBond") # sociable & dominant


#### SRAs: PARTNER SELECTION ####

## using only the categorical selection variable

# impression phase
# imp_sel: "Do you want to be paired with this person to do the decision task?"
R_selection_imp <- RR(imp_sel ~ perceiver.id * target.id | group.id,
                      data = impression_session, na.rm = TRUE, se="LashleyBond")

# cooperation phase
# coord_sel: "Do you want to do the decision task with this person?"
R_selection_coop <- RR(coord_sel ~ perceiver.id * target.id | group.id,
                       data = cooperation_session, na.rm = TRUE, se="LashleyBond")

# collaboration phase
# coll_sel: "Do you want to do the Feud task with this person?"
R_selection_coll <- RR(coll_sel ~ perceiver.id * target.id | group.id,
                       data = collaboration_session, na.rm = TRUE, se="LashleyBond")


## using a variable that combines the categorical selection variable
## and the "strength of selection" variable

# create a variable that combines both into one

impression_session$imp_selStrength[is.na(impression_session$imp_selStrength)] <- 0 # impression session
cooperation_session$coord_selStrength[is.na(cooperation_session$coord_selStrength)] <- 0 # cooperation session
collaboration_session$coll_selStrength[is.na(collaboration_session$coll_selStrength)] <- 0 # collaboration session

# rename the selection variables, give a name that is easier to work with
# original variable names: imp_selStrength, coord_selStrength, coll_selStrength

names(impression_session)[names(impression_session) == "imp_selStrength"] <- "selection"
names(cooperation_session)[names(cooperation_session) == "coord_selStrength"] <- "selection"
names(collaboration_session)[names(collaboration_session) == "coll_selStrength"] <- "selection"

# impression phase
R_selection_imp <- RR(selection ~ perceiver.id * target.id | group.id,
                      data = impression_session, na.rm = TRUE, se="LashleyBond")

# cooperation phase
R_selection_coop <- RR(selection ~ perceiver.id * target.id | group.id,
                       data = cooperation_session, na.rm = TRUE, se="LashleyBond")

# collaboration phase
R_selection_coll <- RR(selection ~ perceiver.id * target.id | group.id,
                       data = collaboration_session, na.rm = TRUE, se="LashleyBond")

#### Create datasets with selection SRA results ####

# add results of selection SRAs into one data frame (target and perceiver effects)

srm_sel_imp <- R_selection_imp$effects # impression phase
srm_sel_coop <- R_selection_coop$effects # cooperation phase
srm_sel_coll <- R_selection_coll$effects # collaboration phase

global_ids_imp <- gsub("^.{0,4}", "", impression_session$globalPID[order(impression_session$group.id)]) # replace first 4 characters with empty string ""
global_ids_imp <- as.numeric(as.character(global_ids_imp)) #convert vector of character values into numeric values
ids_imp <- global_ids_imp[!duplicated(global_ids_imp)] # remove "duplicates" from the participant ID column (duplicates are due to the data format required for SRM analyses)

global_ids_coop <- gsub("^.{0,4}", "", cooperation_session$globalPID[order(cooperation_session$group.id)]) # replace first 4 characters with empty string ""
global_ids_coop <- as.numeric(as.character(global_ids_coop)) #convert vector of character values into numeric values
ids_coop <- global_ids_coop[!duplicated(global_ids_coop)] # remove "duplicates" from the participant ID column (duplicates are due to the data format required for SRM analyses)

ids_coll <- global_ids_coll[!duplicated(global_ids_coll)] # remove "duplicates" from the participant ID column
# add "id" columns to each data frame to indicate the unique participant ID

srm_sel_imp$id <- ids_imp # add participant ids to impression phase results

# COOPERATION PHASE: 57,54, 100 (group 7) and 107, 108, 104 (group 13) excluded from analyses
ids_coop <- ids_coop[! ids_coop%in% c(54,57,100, 104, 107, 108)]# remove the excluded participants
srm_sel_coop$id <- ids_coop # add participant ids to cooperation phase results

srm_sel_coll$id <- ids_coll # add participant ids to impression phase results

# add column names
colnames(srm_sel_imp) <- c("id", "group.id", "sel_imp_p", "sel_imp_t")
colnames(srm_sel_coop) <- c("id", "group.id", "sel_coop_p", "sel_coop_t")
colnames(srm_sel_coll) <- c("id", "group.id", "sel_coll_p", "sel_coll_t")

# merge selection SRA results into one data frame

sel_merged_imp_coop <- merge(srm_sel_imp,srm_sel_coop,by="id",all.x=TRUE, all.y = TRUE) 
sel_tar_per <- merge(sel_merged_imp_coop,srm_sel_coll,by="id",all.x=TRUE, all.y = TRUE)

# remove duplicate columns
sel_tar_per = subset(sel_tar_per, select = -c(group.id.y,group.id) )
names(sel_tar_per)[names(sel_tar_per) == 'group.id.x'] <- 'group.id'

# write the SRA reuslts for selection in a .csv file
write.csv(sel_tar_per, "sel_tar_per.csv")

# merge selection relationship effects into one data frame

# relationship effects
sel_rel_imp <- R_selection_imp$effectsRel
sel_rel_coop <- R_selection_coop$effectsRel
sel_rel_coll <- R_selection_coll$effectsRel


sel_rel_imp <- merge(impression_session,
                     sel_rel_imp, by = c("group.id", "perceiver.id", "target.id"),
                     all.x = TRUE, all.y = TRUE)

sel_rel_coop <- merge(cooperation_session,
                      sel_rel_coop, by = c("group.id", "perceiver.id", "target.id"),
                      all.x = TRUE, all.y = TRUE)

sel_rel_coll <- merge(collaboration_session,
                      sel_rel_coll, by = c("group.id", "perceiver.id", "target.id"),
                      all.x = TRUE, all.y = TRUE)

# create data sets that only have columns of interest
sel_rel_imp <- sel_rel_imp[c("group.id","target.id","perceiver.id",
                             "globalPID", "stageID", "selection",
                             "dyadID", "otherPID", "selfPID",
                             "perspectiveID", "dyad", "relationship")]

sel_rel_coop <- sel_rel_coop[c("group.id","target.id","perceiver.id",
                               "globalPID", "stageID", "selection",
                               "dyadID", "otherPID", "selfPID",
                               "perspectiveID", "dyad", "relationship")]

sel_rel_coll <- sel_rel_coll[c("group.id","target.id","perceiver.id",
                               "globalPID", "stageID", "selection",
                               "dyadID", "otherPID", "selfPID",
                               "perspectiveID", "dyad", "relationship")]

# merge the three datasets into one

sel_rel_1 <- merge(sel_rel_imp, sel_rel_coop, by = c("globalPID","dyadID"),
                   all.x = TRUE, all.y = TRUE)

sel_rel_1 <- sel_rel_1[c("globalPID", "dyadID", "group.id.x", "perceiver.id.x",
                         "target.id.x", "otherPID.x", "selfPID.x", "perspectiveID.x",
                         "dyad.x", "relationship.x", "relationship.y",
                         "selection.x", "selection.y")]

colnames(sel_rel_1) <- c("globalPID", "dyadID", "group.id", "perceiver.id",
                         "target.id", "otherPID", "selfPID", "perspectiveID",
                         "dyad", "rel_sel_imp", "rel_sel_coop", "sel_imp",
                         "sel_coop")

sel_rel <- merge(sel_rel_1, sel_rel_coll, by = c("globalPID", "dyadID"),
                 all.x = TRUE, all.y =TRUE)

sel_rel <- sel_rel[c("globalPID", "dyadID", "group.id.x", "perceiver.id.x",
                     "target.id.x", "otherPID.x", "selfPID.x", "perspectiveID.x",
                     "dyad.x", "rel_sel_imp", "rel_sel_coop", "relationship",
                     "sel_imp", "sel_coop", "selection")]

colnames(sel_rel) <- c("globalPID", "dyadID", "group.id", "perceiver.id",
                       "target.id", "otherPID", "selfPID", "perspectiveID",
                       "dyad", "rel_sel_imp", "rel_sel_coop", "rel_sel_coll",
                       "selimp", "sel_coop", "sel_coll")

sel_rel$group.id <- as.numeric(as.character(sel_rel$group.id))
sel_rel$perceiver.id <- as.numeric(as.character(sel_rel$perceiver.id))
sel_rel$target.id <- as.numeric(as.character(sel_rel$target.id))

write.csv(sel_rel, "sel_rel.csv") # create a .csv file of the relationship effects for selection

#### SRAs: MORALITY, SOCIABILITY, COMPETENCE & LIKING ####

# morality: honest, sincere, trustworthy
# sociability: likable, friendly, sociable
# competence: skillful, intelligent, competent
# liking: likable

# IMPRESSION PHASE

# morality: imp_PP_honM2 (honest), imp_PP_sincM3 (sincere), imp_PP_trustM1 (trustworthy)
# sociability: imp_PP_like (likable), imp_PP_frinS3 (friendly), imp_PP_socS2 (sociable)
# competence: imp_PP_skillC2 (skillful), imp_PP_intC3 (intelligent), imp_PP_compC1 (competent)
# liking: imp_PP_like (likable)

R_moral_imp <- RR(imp_PP_sincM3/imp_PP_trustM1 ~ perceiver.id * target.id | group.id,
                  data = impression_session, na.rm = TRUE,
                  se="LashleyBond") # sincere & trustworthy

R_sociab_imp <- RR(imp_PP_frinS3/imp_PP_socS2 ~ perceiver.id * target.id | group.id,
                   data = impression_session, na.rm = TRUE,
                   se="LashleyBond") # friendly & sociable

R_compet_imp <- RR(imp_PP_skillC2/imp_PP_compC1 ~ perceiver.id * target.id | group.id,
                   data = impression_session, na.rm = TRUE,
                   se="LashleyBond") # skillful & competent

R_like_imp <- RR(imp_PP_like ~ perceiver.id * target.id | group.id,
                 data = impression_session, na.rm = TRUE,
                 se="LashleyBond") # likable

# CONVERSATION PHASE

# morality: conv_PP_honM2 (honest), conv_PP_sincM3 (sincere), conv_PP_trustM1 (trustworthy)
# sociability: conv_PP_like (likable), conv_PP_frinS3 (friendly), conv_PP_socS2 (sociable)
# competence: conv_PP_skillC2 (skillful), conv_PP_intC3 (intelligent), conv_PP_compC1 (competent)
# liking: conv_PP_like (likable)

R_moral_conv <- RR(conv_PP_sincM3/conv_PP_trustM1 ~ perceiver.id * target.id | group.id,
                   data = conversation_session, na.rm = TRUE,
                   se="LashleyBond") # sincere & trustworthy

R_sociab_conv <- RR(conv_PP_frinS3/conv_PP_socS2 ~ perceiver.id * target.id | group.id,
                    data = conversation_session, na.rm = TRUE,
                    se="LashleyBond") # friendly & sociable

R_compet_conv <- RR(conv_PP_skillC2/conv_PP_compC1 ~ perceiver.id * target.id | group.id,
                    data = conversation_session, na.rm = TRUE,
                    se="LashleyBond") # skillful & competent

R_like_conv <- RR(conv_PP_like ~ perceiver.id * target.id | group.id,
                  data = conversation_session, na.rm = TRUE,
                  se="LashleyBond") # likable

# COLLABORATION PHASE

# morality: coll_PP_honM2 (honest), coll_PP_sincM3 (sincere), coll_PP_trustM1 (trustworthy)
# sociability: coll_PP_like (likable), coll_PP_frinS3 (friendly), coll_PP_socS2 (sociable)
# competence: coll_PP_skillC2 (skillful), coll_PP_intC3 (intelligent), coll_PP_compC1 (competent)
# liking: coll_PP_like (likable)

R_moral_coll <- RR(coll_PP_sincM3/coll_PP_trustM1 ~ perceiver.id * target.id | group.id,
                   data = collaboration_session, na.rm = TRUE,
                   se="LashleyBond") # sincere & trustworthy

R_sociab_coll <- RR(coll_PP_frinS3/coll_PP_socS2 ~ perceiver.id * target.id | group.id,
                    data = collaboration_session, na.rm = TRUE,
                    se="LashleyBond") # friendly & sociable

R_compet_coll <- RR(coll_PP_skillC2/coll_PP_compC1 ~ perceiver.id * target.id | group.id,
                    data = collaboration_session, na.rm = TRUE,
                    se="LashleyBond") # skillful & competent

R_like_coll <- RR(coll_PP_like ~ perceiver.id * target.id | group.id,
                  data = collaboration_session, na.rm = TRUE,
                  se="LashleyBond") # likable

#### Create datasets with SRA results (morality, sociability, competence, liking) ####

# merge effects into one data set

# TARGET AND PERCEIVER EFFECTS

srm_moral_imp <- R_moral_imp$effects
srm_sociab_imp <- R_sociab_imp$effects
srm_compet_imp <- R_compet_imp$effects
srm_like_imp <- R_like_imp$effects

srm_moral_conv <- R_moral_conv$effects
srm_sociab_conv <- R_sociab_conv$effects
srm_compet_conv <- R_compet_conv$effects
srm_like_conv <- R_like_conv$effects

srm_moral_coll <- R_moral_coll$effects
srm_sociab_coll <- R_sociab_coll$effects
srm_compet_coll <- R_compet_coll$effects
srm_like_coll <- R_like_coll$effects


# IMPRESSION PHASE

# participants 135, 196, 422, 435, 594, 720 were removed from the analyses due to excessive missing data

global_ids_imp <- gsub("^.{0,4}", "", impression_session$globalPID[order(impression_session$group.id)]) # replace first 4 characters with empty string ""
global_ids_imp <- as.numeric(as.character(global_ids_imp)) #convert vector of character values into numeric values
ids_imp <- global_ids_imp[!duplicated(global_ids_imp)] # remove "duplicates" from the participant ID column (duplicates are due to the data format required for SRM analyses)

ids_imp <- ids_imp[!ids_imp == "135"]
ids_imp <- ids_imp[!ids_imp == "196"]
ids_imp <- ids_imp[!ids_imp == "422"]
ids_imp <- ids_imp[!ids_imp == "435"]
ids_imp <- ids_imp[!ids_imp == "594"]
ids_imp <- ids_imp[!ids_imp == "720"]

# add "id" columns to each data frame to indicate the unique participant ID
effects_imp <- list(srm_moral_imp, srm_sociab_imp, srm_compet_imp, srm_like_imp)
for (i in 1:length(effects_imp)) {
  effects_imp[[i]]$id <- ids_imp }

# combine the SRM effects into one data frame
effects_impression_phase <- bind_cols(effects_imp, id = NULL) # combine all SRM effects into one data frame
effects_impression_phase <- subset(effects_impression_phase,
                                   select = -c(id...5, group.id...6,
                                               id...9, group.id...10,
                                               id...13, group.id...14)) # remove duplicate columns for id and group.id

names(effects_impression_phase)[names(effects_impression_phase) == 'id...1'] <- 'id'
names(effects_impression_phase)[names(effects_impression_phase) == 'group.id...2'] <- 'group.id'

effects_impression_phase$group.id <- as.numeric(as.character(effects_impression_phase$group.id))
write.csv(effects_impression_phase, "tar_per_imp.csv")

# CONVERSATION PHASE

global_ids_conv <- gsub("^.{0,4}", "", conversation_session$globalPID[order(conversation_session$group.id)]) # replace first 4 characters with empty string ""
global_ids_conv <- as.numeric(as.character(global_ids_conv)) #convert vector of character values into numeric values
ids_conv <- global_ids_conv[!duplicated(global_ids_conv)] # remove "duplicates" from the participant ID column (duplicates are due to the data format required for SRM analyses)

# add "id" columns to each data frame to indicate the unique participant ID
effects_conv_2 <- list(srm_moral_conv, srm_sociab_conv, srm_compet_conv, srm_like_conv)
for (i in 1:length(effects_conv_2)) {
  effects_conv_2[[i]]$id <- ids_conv }

# combine the SRM effects into one data frame
effects_conversation_phase_2 <- bind_cols(effects_conv_2, id = NULL) # combine all SRM effects into one data frame
effects_conversation_phase_2 <- subset(effects_conversation_phase_2,
                                       select = -c(id...5, group.id...6,
                                                   id...9, group.id...10,
                                                   id...13, group.id...14)) # remove duplicate columns for id and group.id

names(effects_conversation_phase_2)[names(effects_conversation_phase_2) == 'id...1'] <- 'id'
names(effects_conversation_phase_2)[names(effects_conversation_phase_2) == 'group.id...2'] <- 'group.id'

effects_conversation_phase_2$group.id <- as.numeric(as.character(effects_conversation_phase_2$group.id))
write.csv(effects_conversation_phase_2, "tar_per_conv_2.csv")

# COLLABORATION PHASE

# participant 618 was removed from the analyses due to excessive missing data

global_ids_coll <- gsub("^.{0,4}", "", collaboration_session$globalPID[order(collaboration_session$group.id)]) # replace first 4 characters with empty string ""
global_ids_coll <- as.numeric(as.character(global_ids_coll)) #convert vector of character values into numeric values
ids_coll <- global_ids_coll[!duplicated(global_ids_coll)] # remove "duplicates" from the participant ID column (duplicates are due to the data format required for SRM analyses)

ids_coll <- ids_coll[!ids_coll == "618"]

# add "id" columns to each data frame to indicate the unique participant ID
effects_coll_2 <- list(srm_moral_coll, srm_sociab_coll, srm_compet_coll, srm_like_coll)
for (i in 1:length(effects_coll_2)) {
  effects_coll_2[[i]]$id <- ids_coll }

# combine the SRM effects into one data frame
effects_collaboration_phase_2 <- bind_cols(effects_coll_2, id = NULL) # combine all SRM effects into one data frame
effects_collaboration_phase_2 <- subset(effects_collaboration_phase_2,
                                        select = -c(id...5, group.id...6,
                                                    id...9, group.id...10,
                                                    id...13, group.id...14)) # remove duplicate columns for id and group.id

names(effects_collaboration_phase_2)[names(effects_collaboration_phase_2) == 'id...1'] <- 'id'
names(effects_collaboration_phase_2)[names(effects_collaboration_phase_2) == 'group.id...2'] <- 'group.id'

effects_collaboration_phase_2$group.id <- as.numeric(as.character(effects_collaboration_phase_2$group.id))
write.csv(effects_collaboration_phase_2, "tar_per_coll_2.csv")

# RELATONSHIP EFFECTS

# IMPRESSION PHASE

rel_moral_imp <- R_moral_imp$effectsRel
rel_sociab_imp <- R_sociab_imp$effectsRel
rel_compet_imp <- R_compet_imp$effectsRel
rel_like_imp <- R_like_imp$effectsRel


names(rel_moral_imp)[names(rel_moral_imp) == "imp_PP_trustM1"] <- "target.id"
rel_moral_imp <- merge(impression_session,
                       rel_moral_imp, by = c("group.id", "perceiver.id", "target.id"),
                       all.x = FALSE, all.y = TRUE)

names(rel_sociab_imp)[names(rel_sociab_imp) == "imp_PP_socS2"] <- "target.id"
rel_sociab_imp <- merge(impression_session,
                        rel_sociab_imp, by = c("group.id", "perceiver.id", "target.id"),
                        all.x = FALSE, all.y = TRUE)

names(rel_compet_imp)[names(rel_compet_imp) == "imp_PP_compC1"] <- "target.id"
rel_compet_imp <- merge(impression_session,
                        rel_compet_imp, by = c("group.id", "perceiver.id", "target.id"),
                        all.x = FALSE, all.y = TRUE)

rel_like_imp <- merge(impression_session,
                      rel_like_imp, by = c("group.id", "perceiver.id", "target.id"),
                      all.x = FALSE, all.y = TRUE)

# create data sets that only have columns of interest
rel_moral_imp <- rel_moral_imp[c("group.id","target.id","perceiver.id",
                                 "globalPID", "stageID", "dyadID", "otherPID",
                                 "selfPID", "perspectiveID", "dyad", "relationship")]

rel_sociab_imp <- rel_sociab_imp[c("group.id","target.id","perceiver.id",
                                   "globalPID", "stageID", "dyadID", "otherPID",
                                   "selfPID", "perspectiveID", "dyad", "relationship")]

rel_compet_imp <- rel_compet_imp[c("group.id","target.id","perceiver.id",
                                   "globalPID", "stageID", "dyadID", "otherPID",
                                   "selfPID", "perspectiveID", "dyad", "relationship")]

rel_like_imp <- rel_like_imp[c("group.id","target.id","perceiver.id",
                               "globalPID", "stageID", "dyadID", "otherPID",
                               "selfPID", "perspectiveID", "dyad", "relationship")]


# CONVERSATION PHASE

rel_moral_conv <- R_moral_conv$effectsRel
rel_sociab_conv <- R_sociab_conv$effectsRel
rel_compet_conv <- R_compet_conv$effectsRel
rel_like_conv <- R_like_conv$effectsRel


names(rel_moral_conv)[names(rel_moral_conv) == "conv_PP_trustM1"] <- "target.id"
rel_moral_conv <- merge(conversation_session,
                        rel_moral_conv, by = c("group.id", "perceiver.id", "target.id"),
                        all.x = FALSE, all.y = TRUE)

names(rel_sociab_conv)[names(rel_sociab_conv) == "conv_PP_socS2"] <- "target.id"
rel_sociab_conv <- merge(conversation_session,
                         rel_sociab_conv, by = c("group.id", "perceiver.id", "target.id"),
                         all.x = FALSE, all.y = TRUE)

names(rel_compet_conv)[names(rel_compet_conv) == "conv_PP_compC1"] <- "target.id"
rel_compet_conv <- merge(conversation_session,
                         rel_compet_conv, by = c("group.id", "perceiver.id", "target.id"),
                         all.x = FALSE, all.y = TRUE)

rel_like_conv <- merge(conversation_session,
                       rel_like_conv, by = c("group.id", "perceiver.id", "target.id"),
                       all.x = FALSE, all.y = TRUE)

# create data sets that only have columns of interest
rel_moral_conv <- rel_moral_conv[c("group.id","target.id","perceiver.id",
                                   "globalPID", "stageID", "dyadID", "otherPID",
                                   "selfPID", "perspectiveID", "dyad", "relationship")]

rel_sociab_conv <- rel_sociab_conv[c("group.id","target.id","perceiver.id",
                                     "globalPID", "stageID", "dyadID", "otherPID",
                                     "selfPID", "perspectiveID", "dyad", "relationship")]

rel_compet_conv <- rel_compet_conv[c("group.id","target.id","perceiver.id",
                                     "globalPID", "stageID", "dyadID", "otherPID",
                                     "selfPID", "perspectiveID", "dyad", "relationship")]

rel_like_conv <- rel_like_conv[c("group.id","target.id","perceiver.id",
                                 "globalPID", "stageID", "dyadID", "otherPID",
                                 "selfPID", "perspectiveID", "dyad", "relationship")]


# COLLABORATION PHASE

rel_moral_coll <- R_moral_coll$effectsRel
rel_sociab_coll <- R_sociab_coll$effectsRel
rel_compet_coll <- R_compet_coll$effectsRel
rel_like_coll <- R_like_coll$effectsRel


names(rel_moral_coll)[names(rel_moral_coll) == "coll_PP_trustM1"] <- "target.id"
rel_moral_coll <- merge(collaboration_session,
                        rel_moral_coll, by = c("group.id", "perceiver.id", "target.id"),
                        all.x = FALSE, all.y = TRUE)

names(rel_sociab_coll)[names(rel_sociab_coll) == "coll_PP_socS2"] <- "target.id"
rel_sociab_coll <- merge(collaboration_session,
                         rel_sociab_coll, by = c("group.id", "perceiver.id", "target.id"),
                         all.x = FALSE, all.y = TRUE)

names(rel_compet_coll)[names(rel_compet_coll) == "coll_PP_compC1"] <- "target.id"
rel_compet_coll <- merge(collaboration_session,
                         rel_compet_coll, by = c("group.id", "perceiver.id", "target.id"),
                         all.x = FALSE, all.y = TRUE)

rel_like_coll <- merge(collaboration_session,
                       rel_like_coll, by = c("group.id", "perceiver.id", "target.id"),
                       all.x = FALSE, all.y = TRUE)

# create data sets that only have columns of interest
rel_moral_coll <- rel_moral_coll[c("group.id","target.id","perceiver.id",
                                   "globalPID", "stageID", "dyadID", "otherPID",
                                   "selfPID", "perspectiveID", "dyad", "relationship")]

rel_sociab_coll <- rel_sociab_coll[c("group.id","target.id","perceiver.id",
                                     "globalPID", "stageID", "dyadID", "otherPID",
                                     "selfPID", "perspectiveID", "dyad", "relationship")]

rel_compet_coll <- rel_compet_coll[c("group.id","target.id","perceiver.id",
                                     "globalPID", "stageID", "dyadID", "otherPID",
                                     "selfPID", "perspectiveID", "dyad", "relationship")]

rel_like_coll <- rel_like_coll[c("group.id","target.id","perceiver.id",
                                 "globalPID", "stageID", "dyadID", "otherPID",
                                 "selfPID", "perspectiveID", "dyad", "relationship")]

# COMBINE THE RELATIONSHIP EFFECTS FOR EACH CONSTRUCT IN ORDER TO ASSESS CONSISTENCY

# MORALITY

# merge the three datasets into one

#rel_moral_imp
#rel_moral_conv
#rel_moral_coll

rel_moral_conv$group.id <- as.numeric(as.character(rel_moral_conv$group.id))
rel_moral_conv$perceiver.id <- as.numeric(as.character(rel_moral_conv$perceiver.id))
rel_moral_conv$target.id <- as.numeric(as.character(rel_moral_conv$target.id))

rel_moral_coll$group.id <- as.numeric(as.character(rel_moral_coll$group.id))

rel_moral_imp$perspectiveID<-gsub("imp-","",as.character(rel_moral_imp$perspectiveID))
rel_moral_conv$perspectiveID<-gsub("conv-","",as.character(rel_moral_conv$perspectiveID))
rel_moral_coll$perspectiveID<-gsub("coll-","",as.character(rel_moral_coll$perspectiveID))


rel_moral_1 <- merge(rel_moral_imp, rel_moral_conv, by = c("perspectiveID", "group.id"),
                     all.x = TRUE, all.y = TRUE)

rel_moral_1 <- rel_moral_1[c("group.id", "perspectiveID", "globalPID.x",
                             "dyadID.x", "otherPID.x", "selfPID.x",
                             "relationship.x", "relationship.y")]

colnames(rel_moral_1) <- c("group.id", "perspectiveID", "globalPID",
                           "dyadID", "otherPID", "selfPID",
                           "rel_moral_imp", "rel_moral_conv")

rel_moral <- merge(rel_moral_1, rel_moral_coll, by = c("perspectiveID", "group.id"),
                   all.x = TRUE, all.y = TRUE)

rel_moral <- rel_moral[c("group.id", "perspectiveID", "globalPID.x",
                         "dyadID.x", "otherPID.x", "selfPID.x",
                         "rel_moral_imp", "rel_moral_conv", "relationship")]

colnames(rel_moral) <- c("group.id", "perspectiveID", "globalPID", "dyadID",
                         "otherPID", "selfPID", "rel_moral_imp", "rel_moral_conv",
                         "rel_moral_coll")


write.csv(rel_moral, "rel_moral.csv") # create a .csv file of the relationship effects for morality

# SOCIABILITY

# merge the three datasets into one

#rel_sociab_imp
#rel_sociab_conv
#rel_sociab_coll

rel_sociab_conv$group.id <- as.numeric(as.character(rel_sociab_conv$group.id))
rel_sociab_conv$perceiver.id <- as.numeric(as.character(rel_sociab_conv$perceiver.id))
rel_sociab_conv$target.id <- as.numeric(as.character(rel_sociab_conv$target.id))

rel_sociab_coll$group.id <- as.numeric(as.character(rel_sociab_coll$group.id))

rel_sociab_imp$perspectiveID<-gsub("imp-","",as.character(rel_sociab_imp$perspectiveID))
rel_sociab_conv$perspectiveID<-gsub("conv-","",as.character(rel_sociab_conv$perspectiveID))
rel_sociab_coll$perspectiveID<-gsub("coll-","",as.character(rel_sociab_coll$perspectiveID))


rel_sociab_1 <- merge(rel_sociab_imp, rel_sociab_conv, by = c("perspectiveID", "group.id"),
                      all.x = TRUE, all.y = TRUE)

rel_sociab_1 <- rel_sociab_1[c("group.id", "perspectiveID", "globalPID.x",
                               "dyadID.x", "otherPID.x", "selfPID.x",
                               "relationship.x", "relationship.y")]

colnames(rel_sociab_1) <- c("group.id", "perspectiveID", "globalPID",
                            "dyadID", "otherPID", "selfPID",
                            "rel_sociab_imp", "rel_sociab_conv")

rel_sociab <- merge(rel_sociab_1, rel_sociab_coll, by = c("perspectiveID", "group.id"),
                    all.x = TRUE, all.y = TRUE)

rel_sociab <- rel_sociab[c("group.id", "perspectiveID", "globalPID.x",
                           "dyadID.x", "otherPID.x", "selfPID.x",
                           "rel_sociab_imp", "rel_sociab_conv", "relationship")]

colnames(rel_sociab) <- c("group.id", "perspectiveID", "globalPID", "dyadID",
                          "otherPID", "selfPID", "rel_sociab_imp", "rel_sociab_conv",
                          "rel_sociab_coll")


write.csv(rel_sociab, "rel_sociab.csv") # create a .csv file of the relationship effects for sociability


# COMPETENCE

# merge the three datasets into one

#rel_compet_imp
#rel_compet_conv
#rel_compet_coll

rel_compet_conv$group.id <- as.numeric(as.character(rel_compet_conv$group.id))
rel_compet_conv$perceiver.id <- as.numeric(as.character(rel_compet_conv$perceiver.id))
rel_compet_conv$target.id <- as.numeric(as.character(rel_compet_conv$target.id))

rel_compet_coll$group.id <- as.numeric(as.character(rel_compet_coll$group.id))

rel_compet_imp$perspectiveID<-gsub("imp-","",as.character(rel_compet_imp$perspectiveID))
rel_compet_conv$perspectiveID<-gsub("conv-","",as.character(rel_compet_conv$perspectiveID))
rel_compet_coll$perspectiveID<-gsub("coll-","",as.character(rel_compet_coll$perspectiveID))


rel_compet_1 <- merge(rel_compet_imp, rel_compet_conv, by = c("perspectiveID", "group.id"),
                      all.x = TRUE, all.y = TRUE)

rel_compet_1 <- rel_compet_1[c("group.id", "perspectiveID", "globalPID.x",
                               "dyadID.x", "otherPID.x", "selfPID.x",
                               "relationship.x", "relationship.y")]

colnames(rel_compet_1) <- c("group.id", "perspectiveID", "globalPID",
                            "dyadID", "otherPID", "selfPID",
                            "rel_compet_imp", "rel_compet_conv")

rel_compet <- merge(rel_compet_1, rel_compet_coll, by = c("perspectiveID", "group.id"),
                    all.x = TRUE, all.y = TRUE)

rel_compet <- rel_compet[c("group.id", "perspectiveID", "globalPID.x",
                           "dyadID.x", "otherPID.x", "selfPID.x",
                           "rel_compet_imp", "rel_compet_conv", "relationship")]

colnames(rel_compet) <- c("group.id", "perspectiveID", "globalPID", "dyadID",
                          "otherPID", "selfPID", "rel_compet_imp", "rel_compet_conv",
                          "rel_compet_coll")

write.csv(rel_compet, "rel_compet.csv") # create a .csv file of the relationship effects for competence


# LIKING

# merge the three datasets into one

#rel_like_imp
#rel_like_conv
#rel_like_coll

rel_like_conv$group.id <- as.numeric(as.character(rel_like_conv$group.id))
rel_like_conv$perceiver.id <- as.numeric(as.character(rel_like_conv$perceiver.id))
rel_like_conv$target.id <- as.numeric(as.character(rel_like_conv$target.id))

rel_like_coll$group.id <- as.numeric(as.character(rel_like_coll$group.id))

rel_like_imp$perspectiveID<-gsub("imp-","",as.character(rel_like_imp$perspectiveID))
rel_like_conv$perspectiveID<-gsub("conv-","",as.character(rel_like_conv$perspectiveID))
rel_like_coll$perspectiveID<-gsub("coll-","",as.character(rel_like_coll$perspectiveID))


rel_like_1 <- merge(rel_like_imp, rel_like_conv, by = c("perspectiveID", "group.id"),
                    all.x = TRUE, all.y = TRUE)

rel_like_1 <- rel_like_1[c("group.id", "perspectiveID", "globalPID.x",
                           "dyadID.x", "otherPID.x", "selfPID.x",
                           "relationship.x", "relationship.y")]

colnames(rel_like_1) <- c("group.id", "perspectiveID", "globalPID",
                          "dyadID", "otherPID", "selfPID",
                          "rel_like_imp", "rel_like_conv")

rel_like <- merge(rel_like_1, rel_like_coll, by = c("perspectiveID", "group.id"),
                  all.x = TRUE, all.y = TRUE)

rel_like <- rel_like[c("group.id", "perspectiveID", "globalPID.x",
                       "dyadID.x", "otherPID.x", "selfPID.x",
                       "rel_like_imp", "rel_like_conv", "relationship")]

colnames(rel_like) <- c("group.id", "perspectiveID", "globalPID", "dyadID",
                        "otherPID", "selfPID", "rel_like_imp", "rel_like_conv",
                        "rel_like_coll")

write.csv(rel_like, "rel_like.csv") # create a .csv file of the relationship effects for liking




#### SRAs: WILLINGNESS FOR FUTURE INTERACTION ####

# willingness (only in conversation session): conv_PP_Will

R_willing <- RR(conv_PP_Will ~ perceiver.id * target.id | group.id,
                data = conversation_session, na.rm = TRUE, se="LashleyBond")
R_willing



#### SRAs USING srm PACKAGE ####

# in the SRM analyses below, multiple indicators are used for each construct

# honesty-humility
# we use the one HEXACO item, and the adjectives “honest” and “sincere” 

# conversation phase

### conv_PP_Hex_HH: honesty-humility
### conv_PP_honM2: honest
### conv_PP_sincM3: sincere

# center the variables prior to the analysis to enhance convergence
conversation_session$conv_PP_Hex_HH <- scale(conversation_session$conv_PP_Hex_HH,
                                             center = TRUE, scale = FALSE) #n1
conversation_session$conv_PP_honM2 <- scale(conversation_session$conv_PP_honM2,
                                            center = TRUE, scale = FALSE) #n2
conversation_session$conv_PP_sincM3 <- scale(conversation_session$conv_PP_sincM3,
                                             center = TRUE, scale = FALSE) #n3

## the srm package does not recognize the number of round-robin groups unless the variable is names "Group"
## it also seems to prefer "Actor" and "Partner" as names for the perceiver and target effects
## otherwise, the variable names have to be specified in the function
## the code below can be used to rename these variables as "preferred" by the srm package

names(conversation_session)[names(conversation_session) == "group.id"] <- "Group"
names(conversation_session)[names(conversation_session) == "perceiver.id"] <- "Actor"
names(conversation_session)[names(conversation_session) == "target.id"] <- "Partner"

# define the model:

hh_cfa_conv <- '%Person
	      F@A =~ 1*conv_PP_Hex_HH@A + conv_PP_honM2@A + conv_PP_sincM3@A 
	      F@P =~ 1*conv_PP_Hex_HH@P + conv_PP_honM2@P + conv_PP_sincM3@P 
	      F@A ~~ F@P
	            
	      %Dyad
	      F@AP =~ 1*conv_PP_Hex_HH@AP + conv_PP_honM2@AP + conv_PP_sincM3@AP
	      F@PA =~ 1*conv_PP_Hex_HH@PA + conv_PP_honM2@PA + conv_PP_sincM3@PA
	      F@AP ~~ F@PA
	      '

fit_cfa_hh_conv <- srm(model.syntax = hh_cfa_conv, data = conversation_session,
                       rrgroup_name = "Group",
                       person_names = c("Actor","Partner"),
                       optimizer = "srm", maxiter = 300,
                       conv_par = 1e-04, do_line_search = TRUE)
summary(fit_cfa_hh_conv)


# collaboration phase

### coll_PP_Hex_HH: honesty-humility
### coll_PP_honM2: honest
### coll_PP_sincM3: sincere

collaboration_session$coll_PP_Hex_HH <- scale(collaboration_session$coll_PP_Hex_HH,
                                              center = TRUE, scale = FALSE) #n1
collaboration_session$coll_PP_honM2 <- scale(collaboration_session$coll_PP_honM2,
                                             center = TRUE, scale = FALSE) #n2
collaboration_session$coll_PP_sincM3 <- scale(collaboration_session$coll_PP_sincM3,
                                              center = TRUE, scale = FALSE) #n3

## the srm package does not recognize the number of round-robin groups unless the variable is names "Group"
## it also seems to prefer "Actor" and "Partner" as names for the perceiver and target effects
## otherwise, the variable names have to be specified in the function
## the code below can be used to rename these variables as "preferred" by the srm package

names(collaboration_session)[names(collaboration_session) == "group.id"] <- "Group"
names(collaboration_session)[names(collaboration_session) == "perceiver.id"] <- "Actor"
names(collaboration_session)[names(collaboration_session) == "target.id"] <- "Partner"

# define the model:
hh_cfa_coll <- '%Person
	      F@A =~ 1*coll_PP_Hex_HH@A + coll_PP_honM2@A + coll_PP_sincM3@A 
	      F@P =~ 1*coll_PP_Hex_HH@P + coll_PP_honM2@P + conv_PP_sincM3@P 
	      F@A ~~ F@P
	            
	      %Dyad
	      F@AP =~ 1*coll_PP_Hex_HH@AP + coll_PP_honM2@AP + coll_PP_sincM3@AP
	      F@PA =~ 1*coll_PP_Hex_HH@PA + coll_PP_honM2@PA + coll_PP_sincM3@PA
	      F@AP ~~ F@PA
	      '

fit_cfa_hh_coll <- srm(model.syntax = hh_cfa_coll,
                       data = collaboration_session,
                       rrgroup_name = "Group",
                       person_names = c("Actor","Partner"),
                       optimizer = "srm", maxiter = 300,
                       conv_par = 1e-04, do_line_search = TRUE)
summary(fit_cfa_hh_coll)

# extraversion
# we use the one HEXACO item, and the adjectives "sociable" and "dominant"

# conversation phase

### conv_PP_Hex_HX: extraversion
### conv_PP_socS2: sociable
### conv_PP_dom: dominant

# center the variables prior to the analysis to enhance convergence
conversation_session$conv_PP_Hex_HX <- scale(conversation_session$conv_PP_Hex_HX,
                                             center = TRUE, scale = FALSE) #n1
conversation_session$conv_PP_socS2 <- scale(conversation_session$conv_PP_socS2,
                                            center = TRUE, scale = FALSE) #n2
conversation_session$conv_PP_dom <- scale(conversation_session$conv_PP_dom,
                                          center = TRUE, scale = FALSE) #n3

# define the model:

hx_cfa_conv <- '%Person
	      F@A =~ 1*conv_PP_Hex_HX@A + conv_PP_socS2@A + conv_PP_dom@A 
	      F@P =~ 1*conv_PP_Hex_HX@P + conv_PP_socS2@P + conv_PP_dom@P 
	      F@A ~~ F@P
	            
	      %Dyad
	      F@AP =~ 1*conv_PP_Hex_HX@AP + conv_PP_socS2@AP + conv_PP_dom@AP
	      F@PA =~ 1*conv_PP_Hex_HX@PA + conv_PP_socS2@PA + conv_PP_dom@PA
	      F@AP ~~ F@PA
	      '

fit_cfa_hx_conv <- srm(model.syntax = hx_cfa_conv, data = conversation_session,
                       rrgroup_name = "Group",
                       person_names = c("Actor","Partner"),
                       optimizer = "srm", maxiter = 300,
                       conv_par = 1e-04, do_line_search = TRUE)
summary(fit_cfa_hx_conv)

# collaboration phase

### coll_PP_Hex_HX: extraversion
### coll_PP_socS2: sociable
### coll_PP_dom: dominant

collaboration_session$coll_PP_Hex_HX <- scale(collaboration_session$coll_PP_Hex_HX,
                                              center = TRUE, scale = FALSE) #n1
collaboration_session$coll_PP_socS2 <- scale(collaboration_session$coll_PP_socS2,
                                             center = TRUE, scale = FALSE) #n2
collaboration_session$coll_PP_dom <- scale(collaboration_session$coll_PP_dom,
                                           center = TRUE, scale = FALSE) #n3

# define the model:
hx_cfa_coll <- '%Person
	      F@A =~ 1*coll_PP_Hex_HX@A + coll_PP_socS2@A + coll_PP_dom@A 
	      F@P =~ 1*coll_PP_Hex_HX@P + coll_PP_socS2@P + coll_PP_dom@P 
	      F@A ~~ F@P
	            
	      %Dyad
	      F@AP =~ 1*coll_PP_Hex_HX@AP + coll_PP_socS2@AP + coll_PP_dom@AP
	      F@PA =~ 1*coll_PP_Hex_HX@PA + coll_PP_socS2@PA + coll_PP_dom@PA
	      F@AP ~~ F@PA
	      '

fit_cfa_hx_coll <- srm(model.syntax = hx_cfa_coll, data = collaboration_session,
                       rrgroup_name = "Group",
                       person_names = c("Actor","Partner"),
                       optimizer = "srm", maxiter = 300,
                       conv_par = 1e-04, do_line_search = TRUE)
summary(fit_cfa_hx_coll)





#### WABA RELATIONSHIP EFFECTS: Create the datasets required for the WABA package ####

remotes::install_github("bpoconnor/WABA")
library(WABA)

## PERSONALITY

# honesty-humility
cons_hh <- hexaco_rel[c("group.id", "dyadID", "globalPID", "rel_hh", "rel_hh_coll")]
cons_hh <- na.omit(cons_hh)
global <- gsub("^.{0,4}", "", cons_hh$globalPID) # replace first 4 characters with empty string ""
cons_hh$globalPID <- global
colnames(cons_hh) <- c("group", "dyad", "person", "hh_conv", "hh_coll")

dyad_numbers <-rep(1:(nrow(cons_hh)/2),each=2)
cons_hh$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(cons_hh)/2))
cons_hh$person <- persons

# emotionality
cons_he <- hexaco_rel[c("group.id", "dyadID", "globalPID", "rel_he", "rel_he_coll")]
cons_he <- na.omit(cons_he)
global <- gsub("^.{0,4}", "", cons_he$globalPID) # replace first 4 characters with empty string ""
cons_he$globalPID <- global
colnames(cons_he) <- c("group", "dyad", "person", "he_conv", "he_coll")

dyad_numbers <-rep(1:(nrow(cons_he)/2),each=2)
cons_he$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(cons_he)/2))
cons_he$person <- persons

# extraversion
cons_hx <- hexaco_rel[c("group.id", "dyadID", "globalPID", "rel_hx", "rel_hx_coll")]
cons_hx <- na.omit(cons_hx)
global <- gsub("^.{0,4}", "", cons_hx$globalPID) # replace first 4 characters with empty string ""
cons_hx$globalPID <- global
colnames(cons_hx) <- c("group", "dyad", "person", "hx_conv", "hx_coll")

dyad_numbers <-rep(1:(nrow(cons_hx)/2),each=2)
cons_hx$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(cons_hx)/2))
cons_hx$person <- persons

# agreeableness
cons_ha <- hexaco_rel[c("group.id", "dyadID", "globalPID", "rel_ha", "rel_ha_coll")]
cons_ha <- na.omit(cons_ha)
global <- gsub("^.{0,4}", "", cons_ha$globalPID) # replace first 4 characters with empty string ""
cons_ha$globalPID <- global
colnames(cons_ha) <- c("group", "dyad", "person", "ha_conv", "ha_coll")

dyad_numbers <-rep(1:(nrow(cons_ha)/2),each=2)
cons_ha$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(cons_ha)/2))
cons_ha$person <- persons

# conscientiousness
cons_hc <- hexaco_rel[c("group.id", "dyadID", "globalPID", "rel_hc", "rel_hc_coll")]
cons_hc <- na.omit(cons_hc)
global <- gsub("^.{0,4}", "", cons_hc$globalPID) # replace first 4 characters with empty string ""
cons_hc$globalPID <- global
colnames(cons_hc) <- c("group", "dyad", "person", "hc_conv", "hc_coll")

dyad_numbers <-rep(1:(nrow(cons_hc)/2),each=2)
cons_hc$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(cons_hc)/2))
cons_hc$person <- persons

# openess to experience
cons_ho <- hexaco_rel[c("group.id", "dyadID", "globalPID", "rel_ho", "rel_ho_coll")]
cons_ho <- na.omit(cons_ho)
global <- gsub("^.{0,4}", "", cons_ho$globalPID) # replace first 4 characters with empty string ""
cons_ho$globalPID <- global
colnames(cons_ho) <- c("group", "dyad", "person", "ho_conv", "ho_coll")

dyad_numbers <-rep(1:(nrow(cons_ho)/2),each=2)
cons_ho$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(cons_ho)/2))
cons_ho$person <- persons

## PARTNER SELECTION

selection_rel <- sel_rel[c("group.id", "dyadID", "globalPID", "rel_sel_imp", "rel_sel_coop", "rel_sel_coll")]

global <- gsub("^.{0,4}", "", selection_rel$globalPID) # replace first 4 characters with empty string ""
selection_rel$globalPID <- global
colnames(selection_rel) <- c("group", "dyad", "person", "selection_imp", "selection_coop", "selection_coll")

dyad_numbers <-rep(1:(nrow(selection_rel)/2),each=2)
selection_rel$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(selection_rel)/2))
selection_rel$person <- persons

selection_rel <- na.omit(selection_rel)

## MORALITY

morality_rel <- rel_moral[c("group.id", "dyadID", "globalPID", "rel_moral_imp", "rel_moral_conv", "rel_moral_coll")]
morality_rel <- na.omit(morality_rel)
global <- gsub("^.{0,4}", "", morality_rel$globalPID) # replace first 4 characters with empty string ""
morality_rel$globalPID <- global
colnames(morality_rel) <- c("group", "dyad", "person", "morality_rel_imp", "morality_rel_conv", "morality_rel_coll")

dyad_numbers <-rep(1:(nrow(morality_rel)/2),each=2)
morality_rel$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(morality_rel)/2))
morality_rel$person <- persons

## SOCIABILITY

sociability_rel <- rel_sociab[c("group.id", "dyadID", "globalPID", "rel_sociab_imp", "rel_sociab_conv", "rel_sociab_coll")]
sociability_rel <- na.omit(sociability_rel)
global <- gsub("^.{0,4}", "", sociability_rel$globalPID) # replace first 4 characters with empty string ""
sociability_rel$globalPID <- global
colnames(sociability_rel) <- c("group", "dyad", "person", "sociability_rel_imp", "sociability_rel_conv", "sociability_rel_coll")

dyad_numbers <-rep(1:(nrow(sociability_rel)/2),each=2)
sociability_rel$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(sociability_rel)/2))
sociability_rel$person <- persons

## COMPETENCE

competence_rel <- rel_compet[c("group.id", "dyadID", "globalPID", "rel_compet_imp", "rel_compet_conv", "rel_compet_coll")]
competence_rel <- na.omit(competence_rel)
global <- gsub("^.{0,4}", "", competence_rel$globalPID) # replace first 4 characters with empty string ""
competence_rel$globalPID <- global
colnames(competence_rel) <- c("group", "dyad", "person", "competence_rel_imp", "competence_rel_conv", "competence_rel_coll")

dyad_numbers <-rep(1:(nrow(competence_rel)/2),each=2)
competence_rel$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(competence_rel)/2))
competence_rel$person <- persons

## LIKING

liking_rel <- rel_like[c("group.id", "dyadID", "globalPID", "rel_like_imp", "rel_like_conv", "rel_like_coll")]
liking_rel <- na.omit(liking_rel)
global <- gsub("^.{0,4}", "", liking_rel$globalPID) # replace first 4 characters with empty string ""
liking_rel$globalPID <- global
colnames(liking_rel) <- c("group", "dyad", "person", "liking_rel_imp", "liking_rel_conv", "liking_rel_coll")

dyad_numbers <-rep(1:(nrow(liking_rel)/2),each=2)
liking_rel$dyad <- dyad_numbers
persons <-rep(c(1,2),times=(nrow(liking_rel)/2))
liking_rel$person <- persons

#### WABA HEXACO: Analyses ####

waba(cons_hh)
waba(cons_he)
waba(cons_hx)
waba(cons_ha)
waba(cons_hc)
waba(cons_ho)

#### WABA PARTNER SELECTION: Analyses ####

waba(selection_rel)

#### WABA OTHER VARIABLES: Morality, sociability, competence, liking ####

# WABA for morality, sociability, competence, and liking

waba(morality_rel) # morality
waba(sociability_rel) # sociability
waba(competence_rel) # competence
waba(liking_rel) #liking

#### WABA TARGET EFFECTS: Create the datasets required for the WABA package ####

# HEXACO PERSONALITY

## create datasets that contain, for each hexaco trait, the target effects for the conversation and collaboration phases
## the dataset hexaco_ind contains all this information combined into one dataset

# honesty-humility
hh_target <- hexaco_ind[c("group.id", "conv_PP_Hex_HH.t", "coll_PP_Hex_HH.t")]
hh_target <- na.omit(hh_target)
colnames(hh_target) <- c("group", "hh_conv_target", "hh_coll_target")

# emotionality
he_target <- hexaco_ind[c("group.id", "conv_PP_Hex_HE.t", "coll_PP_Hex_HE.t")]
he_target <- na.omit(he_target)
colnames(he_target) <- c("group", "he_conv_target", "he_coll_target")

# extraversion
hx_target <- hexaco_ind[c("group.id", "conv_PP_Hex_HX.t", "coll_PP_Hex_HX.t")]
hx_target <- na.omit(hx_target)
colnames(hx_target) <- c("group", "hx_conv_target", "hx_coll_target")

# agreeableness
ha_target <- hexaco_ind[c("group.id", "conv_PP_Hex_HA.t", "coll_PP_Hex_HA.t")]
ha_target <- na.omit(ha_target)
colnames(ha_target) <- c("group", "ha_conv_target", "ha_coll_target")

# conscientiousness
hc_target <- hexaco_ind[c("group.id", "conv_PP_Hex_HC.t", "coll_PP_Hex_HC.t")]
hc_target <- na.omit(hc_target)
colnames(hc_target) <- c("group", "hc_conv_target", "hc_coll_target")

# openness to experience
ho_target <- hexaco_ind[c("group.id", "conv_PP_Hex_HO.t", "coll_PP_Hex_HO.t")]
ho_target <- na.omit(ho_target)
colnames(ho_target) <- c("group", "ho_conv_target", "ho_coll_target")

# PARTNER SELECTION

## the dataset sel_tar_per contains all the necessary variables, combined into one dataset

selection_target <- sel_tar_per[c("group.id", "sel_imp_t", "sel_coop_t", "sel_coll_t")]
selection_target <- na.omit(selection_target)
colnames(selection_target) <- c("group", "selection_imp_target", "selection_coop_target", "selection_coll_target")

# MORALITY, SOCIABILITY, COMPETENCE, AND LIKING

# first, create the common dataset that contains all necessary variables

target_effects_1 <- merge(effects_impression_phase, effects_conversation_phase_2,
                          by = c("id", "group.id"), all.x = TRUE, all.y = TRUE)

target_effects <- merge(target_effects_1, effects_collaboration_phase_2,
                        by = c("id", "group.id"), all.x = TRUE, all.y = TRUE)

# MORALITY

# effects_impression_phase
# effects_conversation_phase_2
# effects_collaboration_phase_2

morality_target <- target_effects[c("group.id", "imp_PP_sincM3.t", "conv_PP_sincM3.t", "coll_PP_sincM3.t")]
morality_target <- na.omit(morality_target)
colnames(morality_target) <- c("group", "morality_imp_target", "morality_conv_target", "morality_coll_target")

# SOCIABILITY

sociability_target <- target_effects[c("group.id", "imp_PP_frinS3.t", "conv_PP_frinS3.t", "coll_PP_frinS3.t")]
sociability_target <- na.omit(sociability_target)
colnames(sociability_target) <- c("group", "sociability_imp_target", "sociability_conv_target", "sociability_coll_target")

# COMPETENCE

competence_target <- target_effects[c("group.id", "imp_PP_skillC2.t", "conv_PP_skillC2.t", "coll_PP_skillC2.t")]
competence_target <- na.omit(competence_target)
colnames(competence_target) <- c("group", "sociability_imp_target", "competence_conv_target", "competence_coll_target")

# LIKING

liking_target <- target_effects[c("group.id", "imp_PP_like.t", "conv_PP_like.t", "coll_PP_like.t")]
liking_target <- na.omit(liking_target)
colnames(liking_target) <- c("group", "liking_imp_target", "liking_conv_target", "liking_coll_target")


#### WABA TARGET EFFECTS: Analyses ####

# HEXACO PERSONALITY
waba(hh_target) # honesty-humility
waba(he_target) # emotionality
waba(hx_target) # extraversion
waba(ha_target) # agreeableness
waba(hc_target) # conscientiousness
waba(ho_target) # openness to experience

# PARTNER SELECTION
selection_target$group <- as.numeric(as.character(selection_target$group))
waba(selection_target)

# MORALITY
waba(morality_target)

# SOCIABILITY
waba(sociability_target)

# COMPETENCE
waba(competence_target)

# LIKING
waba(liking_target)

#### CHECK WHETHER PARTICIPANT IDS AFFECT CONSISTENCY ESTIMATES ####

## HEXACO PERSONALITY

# Create a dataset that includes participant ID
consistency <- hexaco_rel
# global <- gsub("^.{0,4}", "", hexaco_rel$globalPID) # replace first 4 characters with empty string ""
dyad_numbers <-rep(1:(nrow(consistency)/2),each=2)
consistency$dyad <- dyad_numbers

## HONESTY-HUMILITY

# model without IDs
model_hh_1 <- lmerTest::lmer(rel_hh_coll ~ rel_hh + (1|group.id), data = consistency)
summary(model_hh_1)
ranova(model_hh_1)

library(performance)
performance::icc(model_hh_1, ci = TRUE)
performance::icc(model_hh_1, by_group = TRUE)

# model with IDs
model_hh_2 <- lmerTest::lmer(rel_hh_coll ~ rel_hh + (1 | group.id) + (1 | globalPID), data = consistency)
summary(model_hh_2)
ranova(model_hh_2)

performance::icc(model_hh_2, ci = TRUE)
performance::icc(model_hh_2, by_group = TRUE) # this will tell us how much fixed effect changes if ids are accounted for

anova(model_hh_1, model_hh_2)

# Repeat procedure for dyads as the clustering factor

# model without IDs
model_hh_3 <- lmer(rel_hh_coll ~ rel_hh + (1|group.id/dyad), data = consistency)
summary(model_hh_3)
ranova(model_hh_3)

performance::icc(model_hh_3, ci = TRUE)
performance::icc(model_hh_3, by_group = TRUE)

# model with IDs
model_hh_4 <- lmer(rel_hh_coll ~ rel_hh + (1|group.id/dyad) + (1|globalPID), data = consistency)
summary(model_hh_4)
ranova(model_hh_4)

performance::icc(model_hh_4, ci = TRUE)
performance::icc(model_hh_4, by_group = TRUE)

anova(model_hh_3, model_hh_4)

## EMOTIONALITY

# model without IDs
model_he_1 <- lmerTest::lmer(rel_he_coll ~ rel_he + (1|group.id), data = consistency)
summary(model_he_1)
ranova(model_he_1)

library(performance)
performance::icc(model_hh_1, ci = TRUE)
performance::icc(model_hh_1, by_group = TRUE)

# model with IDs
model_he_2 <- lmerTest::lmer(rel_he_coll ~ rel_he + (1 | group.id) + (1 | globalPID), data = consistency)
summary(model_he_2)
ranova(model_he_2)

performance::icc(model_he_2, ci = TRUE)
performance::icc(model_he_2, by_group = TRUE) # this will tell us how much fixed effect changes if ids are accounted for

anova(model_he_1, model_he_2)

# Repeat procedure for dyads as the clustering factor

# model without IDs
model_he_3 <- lmer(rel_he_coll ~ rel_he + (1|group.id/dyad), data = consistency)
summary(model_he_3)
ranova(model_he_3)

performance::icc(model_he_3, ci = TRUE)
performance::icc(model_he_3, by_group = TRUE)

# model with IDs
model_he_4 <- lmer(rel_he_coll ~ rel_he + (1|group.id/dyad) + (1|globalPID), data = consistency)
summary(model_he_4)
ranova(model_he_4)

performance::icc(model_he_4, ci = TRUE)
performance::icc(model_he_4, by_group = TRUE)

anova(model_he_3, model_he_4)

## EXTRAVERSION

# model without IDs
model_hx_1 <- lmerTest::lmer(rel_hx_coll ~ rel_hx + (1|group.id), data = consistency)
summary(model_hx_1)
ranova(model_hx_1)

library(performance)
performance::icc(model_hx_1, ci = TRUE)
performance::icc(model_hx_1, by_group = TRUE)

# model with IDs
model_hx_2 <- lmerTest::lmer(rel_hx_coll ~ rel_hx + (1 | group.id) + (1 | globalPID), data = consistency)
summary(model_hx_2)
ranova(model_hx_2)

performance::icc(model_hx_2, ci = TRUE)
performance::icc(model_hx_2, by_group = TRUE) # this will tell us how much fixed effect changes if ids are accounted for

anova(model_hx_1, model_hx_2)

# Repeat procedure for dyads as the clustering factor

# model without IDs
model_hx_3 <- lmer(rel_hx_coll ~ rel_hx + (1|group.id/dyad), data = consistency)
summary(model_hx_3)
ranova(model_hx_3)

performance::icc(model_hx_3, ci = TRUE)
performance::icc(model_hx_3, by_group = TRUE)

# model with IDs
model_hx_4 <- lmer(rel_hx_coll ~ rel_hx + (1|group.id/dyad) + (1|globalPID), data = consistency)
summary(model_hx_4)
ranova(model_hx_4)

performance::icc(model_hx_4, ci = TRUE)
performance::icc(model_hx_4, by_group = TRUE)

anova(model_hx_3, model_hx_4)

## AGREEABLENESS

# model without IDs
model_ha_1 <- lmerTest::lmer(rel_ha_coll ~ rel_ha + (1|group.id), data = consistency)
summary(model_ha_1)
ranova(model_ha_1)

library(performance)
performance::icc(model_ha_1, ci = TRUE)
performance::icc(model_ha_1, by_group = TRUE)

# model with IDs
model_ha_2 <- lmerTest::lmer(rel_ha_coll ~ rel_ha + (1 | group.id) + (1 | globalPID), data = consistency)
summary(model_ha_2)
ranova(model_ha_2)

performance::icc(model_ha_2, ci = TRUE)
performance::icc(model_ha_2, by_group = TRUE) # this will tell us how much fixed effect changes if ids are accounted for

anova(model_ha_1, model_ha_2)

# Repeat procedure for dyads as the clustering factor

# model without IDs
model_ha_3 <- lmer(rel_ha_coll ~ rel_ha + (1|group.id/dyad), data = consistency)
summary(model_ha_3)
ranova(model_ha_3)

performance::icc(model_ha_3, ci = TRUE)
performance::icc(model_ha_3, by_group = TRUE)

# model with IDs
model_ha_4 <- lmer(rel_ha_coll ~ rel_ha + (1|group.id/dyad) + (1|globalPID), data = consistency)
summary(model_ha_4)
ranova(model_ha_4)

performance::icc(model_ha_4, ci = TRUE)
performance::icc(model_ha_4, by_group = TRUE)

anova(model_ha_3, model_ha_4)

## CONSCIENTIOUSNESS

# model without IDs
model_hc_1 <- lmerTest::lmer(rel_hc_coll ~ rel_hc + (1|group.id), data = consistency)
summary(model_hc_1)
ranova(model_hc_1)

library(performance)
performance::icc(model_hc_1, ci = TRUE)
performance::icc(model_hc_1, by_group = TRUE)

# model with IDs
model_hc_2 <- lmerTest::lmer(rel_hc_coll ~ rel_hc + (1 | group.id) + (1 | globalPID), data = consistency)
summary(model_hc_2)
ranova(model_hc_2)

performance::icc(model_hc_2, ci = TRUE)
performance::icc(model_hc_2, by_group = TRUE) # this will tell us how much fixed effect changes if ids are accounted for

anova(model_hc_1, model_hc_2)

# Repeat procedure for dyads as the clustering factor

# model without IDs
model_hc_3 <- lmer(rel_hc_coll ~ rel_hc + (1|group.id/dyad), data = consistency)
summary(model_hc_3)
ranova(model_hc_3)

performance::icc(model_hc_3, ci = TRUE)
performance::icc(model_hc_3, by_group = TRUE)

# model with IDs
model_hc_4 <- lmer(rel_hc_coll ~ rel_hc + (1|group.id/dyad) + (1|globalPID), data = consistency)
summary(model_hc_4)
ranova(model_hc_4)

performance::icc(model_hc_4, ci = TRUE)
performance::icc(model_hc_4, by_group = TRUE)

anova(model_hc_3, model_hc_4)

## OPENNESS TO EXPERIENCE

# model without IDs
model_ho_1 <- lmerTest::lmer(rel_ho_coll ~ rel_ho + (1|group.id), data = consistency)
summary(model_ho_1)
ranova(model_ho_1)

library(performance)
performance::icc(model_ho_1, ci = TRUE)
performance::icc(model_ho_1, by_group = TRUE)

# model with IDs
model_ho_2 <- lmerTest::lmer(rel_ho_coll ~ rel_ho + (1 | group.id) + (1 | globalPID), data = consistency)
summary(model_ho_2)
ranova(model_ho_2)

performance::icc(model_ho_2, ci = TRUE)
performance::icc(model_ho_2, by_group = TRUE) # this will tell us how much fixed effect changes if ids are accounted for

anova(model_ho_1, model_ho_2)

# Repeat procedure for dyads as the clustering factor

# model without IDs
model_ho_3 <- lmer(rel_ho_coll ~ rel_ho + (1|group.id/dyad), data = consistency)
summary(model_ho_3)
ranova(model_ho_3)

performance::icc(model_ho_3, ci = TRUE)
performance::icc(model_ho_3, by_group = TRUE)

# model with IDs
model_ho_4 <- lmer(rel_ho_coll ~ rel_ho + (1|group.id/dyad) + (1|globalPID), data = consistency)
summary(model_ho_4)
ranova(model_ho_4)

performance::icc(model_ho_4, ci = TRUE)
performance::icc(model_ho_4, by_group = TRUE)

anova(model_ho_3, model_ho_4)




# HYPOTHESES TESTS --------------------------------------------------------

# setting up packages
library(lme4) # for the analysis
library(tidyverse) # needed for data manipulation
library(RColorBrewer) # needed for some extra colors in graphs
library(lmerTest) # to get p-value estimations that are not part of the standard lme4 package
library(viridis) # to make nicer color plots

# this can be used to load the dataset right away, so it's not necessary to run all the analyses above every time
# read.csv("selection_personality.csv")
selection_personality <- merge(hexaco_ind, sel_tar_per, by = c("id", "group.id"),
                               all.x = FALSE, all.y = FALSE)


#### HYPOTHESIS 1 ####

## Relationship between target and relational HONESTY-HUMILITY, and partner selection for a cooperation task
## a. Target honesty-humility is positively related to partner selection for a cooperation task
## b. Relational honesty-humility is positively related to partner selection for a cooperation task

## these relations are examined at the collaboration phase

## a. relation between target effect for honesty-humility and target effect for partner selection

selection_personality <- merge(hexaco_ind, sel_tar_per, by = c("id", "group.id"),
                               all.x = FALSE, all.y = FALSE)

# before the analysis, we can plot the relation between target hh and selection
# without taking into account the multilevel structure of the data
# and also add a regression line to the plot
ggplot(data = selection_personality,
       aes(x = conv_PP_Hex_HH.t,
           y = sel_coll_t)) +
  geom_point(size = 1.2,
             alpha = .8) +
  geom_smooth(method = lm,
              se = FALSE,
              col = "black",
              linewidth = .5,
              alpha = .8) + # to add regression line
  theme_minimal() +
  labs(title = "Relation between target honesty-humility and target partner selection")

# from the graph, there seems to be a positive relation between hh and selection

# we can show the multilevel structure of the data by color-coding the groups
# and draw different regression lines for the groups in the dataset
ggplot(data = selection_personality,
       aes(x = conv_PP_Hex_HH.t,
           y = sel_coll_t,
           col = group.id,
           group = group.id)) +
  geom_point(size = 1.2,
             alpha = .8) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(100)) +
  geom_smooth(method = lm,
              se = FALSE,
              linewidth = .5,
              alpha = .5) +
  labs(title = "Relation between target honesty-humility and target partner selection")

## intercept-only model

## in the first model, the intercept randomly varies across groups
## but we do not include predictors to explain this variability

# the dependent variable "selection" is predicted by an intercept and a random error term for the intercept
model1 <- lmer(sel_coll_t ~ 1 + (1|group.id),
               data = selection_personality, REML = FALSE)
summary(model1)

## first-level predictor

# add the first-level predictor, target honesty-humility
# for now, we assure the effect is fixed across the groups
model2 <- lmer(sel_coll_t ~ 1 + conv_PP_Hex_HH.t + (1|group.id),
               data = selection_personality, REML = FALSE)
summary(model2)

anova(model1, model2)

## first-level predictor with a random slope

# include the random slope for honesty-humility
model3 <- lmer(sel_coll_t ~ 1 + conv_PP_Hex_HH.t + (1 + conv_PP_Hex_HH.t|group.id),
               data = selection_personality, REML = FALSE)
summary(model3) # this model fails to converge

## b. relation between relational honesty-humility and relational partner selection

# create the necessary data set

rel_coll <- read.csv('rel_coll.csv') # hexaco relationship effects for the collaboration phase
sel_rel <- read.csv("sel_rel.csv") # selection relationship effects

rel_coll$perspectiveID <- gsub("^.{0,5}", "", rel_coll$perspectiveID)
sel_rel$perspectiveID <- gsub("^.{0,4}", "", sel_rel$perspectiveID)

selection_personality_rel <- merge(sel_rel, rel_coll, by = "perspectiveID",
                                   all.x = TRUE, all.y = TRUE)

selection_personality_rel <- na.omit(selection_personality_rel)


# plot the relation between relational hh and selection
# add a regression line to the plot
ggplot(data = selection_personality_rel,
       aes(x = rel_hh_coll,
           y = rel_sel_coll)) +
  geom_point(size = 1.2,
             alpha = .8) +
  geom_smooth(method = lm,
              se = FALSE,
              col = "black",
              linewidth = .5,
              alpha = .8) + # to add regression line
  theme_minimal() +
  labs(title = "Relation between relational honesty-humility and relational partner selection")

# draw color-coded regression lines for the groups in the dataset
ggplot(data = selection_personality_rel,
       aes(x = rel_hh_coll,
           y = rel_sel_coll,
           col = group.id.x,
           group = group.id.x)) +
  geom_point(size = 1.2,
             alpha = .8) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(100)) +
  geom_smooth(method = lm,
              se = FALSE,
              linewidth = .5,
              alpha = .5) +
  labs(title = "Relation between relational honesty-humility and partner selection")

## intercept-only model

## in the first model, the intercept randomly varies across groups
## but we do not include predictors to explain this variability

# the dependent variable "selection" is predicted by an intercept and a random error term for the intercept
model1_rel <- lmer(rel_sel_coll ~ 1 + (1|group.id.x),
               data = selection_personality_rel, REML = FALSE)
summary(model1_rel)

## first-level predictor

# add the first-level predictor, relational honesty-humility
model2_rel <- lmer(rel_sel_coll ~ 1 + rel_hh_coll + (1|group.id.x),
               data = selection_personality_rel, REML = FALSE)
summary(model2_rel)

anova(model1_rel, model2_rel)

## first-level predictor with a random slope

# include the random slope for relational honesty-humility
model3_rel <- lmer(rel_sel_coll ~ 1 + rel_hh_coll + (1 + rel_hh_coll|group.id.x),
               data = selection_personality_rel, REML = FALSE)
summary(model3_rel) # this model fails to converge


#### HYPOTHESIS 2 ####

## Relationship between target and relational EXTRAVERSION, and partner selection for a cooperation task
## a. Target extraversion is positively related to partner selection for a cooperation task
## b. Relational extraversion is positively related to partner selection for a cooperation task

## these relations are examined at the collaboration phase

## a. relation between target effect for extraversion and target effect for partner selection

# before the analysis, we can plot the relation between target hx and selection
# without taking into account the multilevel structure of the data
# and also add a regression line to the plot
ggplot(data = selection_personality,
       aes(x = conv_PP_Hex_HX.t,
           y = sel_coll_t)) +
  geom_point(size = 1.2,
             alpha = .8) +
  geom_smooth(method = lm,
              se = FALSE,
              col = "black",
              linewidth = .5,
              alpha = .8) + # to add regression line
  theme_minimal() +
  labs(title = "Relation between target extraversion and target partner selection")

# draw different-colored regression lines for the groups in the dataset
ggplot(data = selection_personality,
       aes(x = conv_PP_Hex_HX.t,
           y = sel_coll_t,
           col = group.id,
           group = group.id)) +
  geom_point(size = 1.2,
             alpha = .8) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(100)) +
  geom_smooth(method = lm,
              se = FALSE,
              linewidth = .5,
              alpha = .5) +
  labs(title = "Relation between target extraversion and target partner selection")

## intercept-only model

# the dependent variable "selection" is predicted by an intercept and a random error term for the intercept
model1 <- lmer(sel_coll_t ~ 1 + (1|group.id),
               data = selection_personality, REML = FALSE)
summary(model1)

## first-level predictor

# add the first-level predictor, target extraversion
# for now, we assure the effect is fixed across the groups
model4 <- lmer(sel_coll_t ~ 1 + conv_PP_Hex_HX.t + (1|group.id),
               data = selection_personality, REML = FALSE)
summary(model4)

## first-level predictor with a random slope
# include the random slope for extraversion
model5 <- lmer(sel_coll_t ~ 1 + conv_PP_Hex_HX.t + (1 + conv_PP_Hex_HX.t|group.id),
               data = selection_personality, REML = FALSE)
summary(model5) # this model fails to converge

anova(model1, model4)
anova(model4, model5)

## b. relation between relational extraversion and relational partner selection

# plot the relation between relational extraversion and selection
# add a regression line to the plot
ggplot(data = selection_personality_rel,
       aes(x = rel_hx_coll,
           y = rel_sel_coll)) +
  geom_point(size = 1.2,
             alpha = .8) +
  geom_smooth(method = lm,
              se = FALSE,
              col = "black",
              linewidth = .5,
              alpha = .8) + # to add regression line
  theme_minimal() +
  labs(title = "Relation between relational extraversion and relational partner selection")

# draw color-coded regression lines for the groups in the dataset
ggplot(data = selection_personality_rel,
       aes(x = rel_hx_coll,
           y = rel_sel_coll,
           col = group.id.x,
           group = group.id.x)) +
  geom_point(size = 1.2,
             alpha = .8) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(100)) +
  geom_smooth(method = lm,
              se = FALSE,
              linewidth = .5,
              alpha = .5) +
  labs(title = "Relation between relational extraversion and partner selection")

## intercept-only model

## in the first model, the intercept randomly varies across groups
## but we do not include predictors to explain this variability

# the dependent variable "selection" is predicted by an intercept and a random error term for the intercept
model1_rel <- lmer(rel_sel_coll ~ 1 + (1|group.id.x),
                   data = selection_personality_rel, REML = FALSE)
summary(model1_rel)

## first-level predictor

# add the first-level predictor, relational honesty-humility
model4_rel <- lmer(rel_sel_coll ~ 1 + rel_hx_coll + (1|group.id.x),
                   data = selection_personality_rel, REML = FALSE)
summary(model4_rel)

anova(model1_rel, model4_rel)

## first-level predictor with a random slope

# include the random slope for relational honesty-humility
model5_rel <- lmer(rel_sel_coll ~ 1 + rel_hx_coll + (1 + rel_hx_coll|group.id.x),
                   data = selection_personality_rel, REML = FALSE)
summary(model5_rel) # this model fails to converge

anova(model4_rel, model5_rel)


#### HYPOTHESIS 3 ####

## Relationship between target and relational COMPETENCE, and partner selection for a cooperation task
## a. Target competence is positively related to partner selection for a cooperation task
## b. Relational competence is positively related to partner selection for a cooperation task

## these relations are examined at the collaboration phase

tar_per_coll_2 <- read.csv("tar_per_coll_2.csv")
selection_competence <- merge(tar_per_coll_2, sel_tar_per, by = c("id", "group.id"),
                               all.x = FALSE, all.y = FALSE)

## a. relation between target effect for competence and target effect for partner selection

# plot the relation between target competence and selection
# without taking into account the multilevel structure of the data
# and add a regression line to the plot
ggplot(data = selection_competence,
       aes(x = coll_PP_skillC2.t,
           y = sel_coll_t)) +
  geom_point(size = 1.2,
             alpha = .8) +
  geom_smooth(method = lm,
              se = FALSE,
              col = "black",
              linewidth = .5,
              alpha = .8) + # to add regression line
  theme_minimal() +
  labs(title = "Relation between target competence and target partner selection")

# draw different-colored regression lines for the groups in the dataset
ggplot(data = selection_competence,
       aes(x = coll_PP_skillC2.t,
           y = sel_coll_t,
           col = group.id,
           group = group.id)) +
  geom_point(size = 1.2,
             alpha = .8) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(100)) +
  geom_smooth(method = lm,
              se = FALSE,
              linewidth = .5,
              alpha = .5) +
  labs(title = "Relation between target competence and target partner selection")

## intercept-only model

# the dependent variable "selection" is predicted by an intercept and a random error term for the intercept
model1 <- lmer(sel_coll_t ~ 1 + (1|group.id),
               data = selection_personality, REML = FALSE)
summary(model1)

## first-level predictor

# add the first-level predictor, target competence
# for now, we assure the effect is fixed across the groups
model6 <- lmer(sel_coll_t ~ 1 + coll_PP_skillC2.t + (1|group.id),
               data = selection_competence, REML = FALSE)
summary(model6)

## first-level predictor with a random slope
# include the random slope for competence
model7 <- lmer(sel_coll_t ~ 1 + coll_PP_skillC2.t + (1 + coll_PP_skillC2.t|group.id),
               data = selection_competence, REML = FALSE)
summary(model7) # this model fails to converge

anova(model1, model6)
anova(model6, model7)

## b. relation between relational competence and relational partner selection


# create the necessary data set

rel_compet <- read.csv("rel_compet.csv") # competence relationship effects
sel_rel <- read.csv("sel_rel.csv") # selection relationship effects

rel_compet <- na.omit(rel_compet)
sel_rel <- na.omit(sel_rel)

selection_competence_rel <- merge(sel_rel, rel_compet, by = "perspectiveID",
                                   all.x = TRUE, all.y = TRUE)

selection_competence_rel <- na.omit(selection_competence_rel)


# plot the relation between relational competence and selection
# add a regression line to the plot
ggplot(data = selection_competence_rel,
       aes(x = rel_compet_coll,
           y = rel_sel_coll)) +
  geom_point(size = 1.2,
             alpha = .8) +
  geom_smooth(method = lm,
              se = FALSE,
              col = "black",
              linewidth = .5,
              alpha = .8) + # to add regression line
  theme_minimal() +
  labs(title = "Relation between relational competence and relational partner selection")

# draw color-coded regression lines for the groups in the dataset
ggplot(data = selection_competence_rel,
       aes(x = rel_compet_coll,
           y = rel_sel_coll,
           col = group.id.x,
           group = group.id.x)) +
  geom_point(size = 1.2,
             alpha = .8) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(100)) +
  geom_smooth(method = lm,
              se = FALSE,
              linewidth = .5,
              alpha = .5) +
  labs(title = "Relation between relational competence and partner selection")

## intercept-only model

# the dependent variable "selection" is predicted by an intercept and a random error term for the intercept
model1_rel <- lmer(rel_sel_coll ~ 1 + (1|group.id.x),
                   data = selection_competence_rel, REML = FALSE)
summary(model1_rel)

## first-level predictor

# add the first-level predictor, relational competence
model6_rel <- lmer(rel_sel_coll ~ 1 + rel_compet_coll + (1|group.id.x),
                   data = selection_competence_rel, REML = FALSE)
summary(model6_rel)

anova(model1_rel, model6_rel)

## first-level predictor with a random slope

# include the random slope for relational competence
model7_rel <- lmer(rel_sel_coll ~ 1 + rel_compet_coll + (1 + rel_compet_coll|group.id.x),
                   data = selection_competence_rel, REML = FALSE)
summary(model7_rel) # this model fails to converge

anova(model6_rel, model7_rel)

#### HYPOTHESIS 4 ####

## Moderating role of task type (trust-based versus competence-based)
## on the relationship between HONESTY-HUMILITY and COMPETENCE, and PARTNER SELECTION for a cooperation task

## a. The positive relation between target and relational HONESTY-HUMILITY, and partner selection is moderated by task type
## b.	The positive relation between target and relational COMPETENCE, and partner selection is moderated by task type

# a1. relation between target honesty-humility and target selection, moderated by task type

## create the necessary dataset

# target hh & target_selection: selection_personality
# task type: cooperation session

cooperation_session$globalPID <- gsub("^.{0,4}", "", cooperation_session$globalPID) # Replace first 5 characters with empty string ""
names(cooperation_session)[names(cooperation_session) == "globalPID"] <- "id"
cooperation_session <- cooperation_session[ -c(1) ]

mod_selection_personality <- merge(selection_personality, cooperation_session,
                                   by = c("id"), all.x = TRUE, all.y = FALSE)

mod_selection_personality <- mod_selection_personality[!duplicated(mod_selection_personality$id),]
mod_selection_personality = subset(mod_selection_personality,
                                   select = -c(roundIdx,coordC_remindYN, group.id.y,
                                               coordW_remindYN, coord_itemOrderID))
names(mod_selection_personality)[names(mod_selection_personality) == "group.id.x"] <- "group.id"

# perform the analysis

# the dependent variable "selection" is predicted by an intercept and a random error term for the intercept
model1 <- lmer(sel_coll_t ~ 1 + (1|group.id),
               data = mod_selection_personality, REML = FALSE)
summary(model1)

na.omit(mod_selection_personality)

## first-level predictor

# add the first-level predictors, target honesty-humility and task-type
# for now, we assure the effect is fixed across the groups
model8 <- lmer(sel_coll_t ~ 1 + conv_PP_Hex_HH.t + coord_taskType + (1|group.id),
               data = mod_selection_personality, REML = FALSE)
summary(model8)

anova(model1, model8)

## first-level predictor with a random slope: target hh
# include the random slope for target honesty-humility
model9 <- lmer(sel_coll_t ~ 1 + conv_PP_Hex_HH.t + coord_taskType + (1 + conv_PP_Hex_HH.t|group.id),
               data = mod_selection_personality, REML = FALSE)
summary(model9)

# add the interaction term
model10 <- lmer(sel_coll_t ~ 1 + conv_PP_Hex_HH.t + coord_taskType +
                  conv_PP_Hex_HH.t:coord_taskType + (1 + conv_PP_Hex_HH.t|group.id),
                data = mod_selection_personality, REML = FALSE)
summary(model10)

# a2. relation between relational honesty-humility and relational selection, moderated by task type

# honesty-humility and selection: selection_personality_rel
# task type: cooperation session

cooperation_session$perspectiveID <- gsub("^.{0,6}", "", cooperation_session$perspectiveID)
mod_selection_personality_rel <- merge(selection_personality_rel, cooperation_session,
                                       by = c("perspectiveID"), all.x = TRUE, all.y = FALSE)

# perform the analysis

# the dependent variable "selection" is predicted by an intercept and a random error term for the intercept
model1_rel <- lmer(rel_sel_coll ~ 1 + (1|group.id),
               data = mod_selection_personality_rel, REML = FALSE)
summary(model1_rel)

## first-level predictor

# add the first-level predictors, relational honesty-humility and task-type
# for now, we assure the effect is fixed across the groups
model8_rel <- lmer(rel_sel_coll ~ 1 + rel_hh_coll + coord_taskType + (1|group.id),
                   data = mod_selection_personality_rel, REML = FALSE)
summary(model8_rel)

anova(model1_rel, model8_rel)

## first-level predictor with a random slope: relational hh
# include the random slope for target honesty-humility
model9_rel <- lmer(rel_sel_coll ~ 1 + rel_hh_coll + coord_taskType + (1 + rel_hh_coll|group.id),
               data = mod_selection_personality_rel, REML = FALSE)
summary(model9_rel) # model fails to converge

# add the interaction term
model10_rel <- lmer(rel_sel_coll ~ 1 + rel_hh_coll + coord_taskType +
                      rel_hh_coll:coord_taskType + (1 + rel_hh_coll|group.id),
                    data = mod_selection_personality_rel, REML = FALSE)
summary(model10_rel) # model fails to converge

# RESEARCH QUESTIONS ------------------------------------------------------


#### RESEARCH QUESTION 2

## Relationship between target and relational personality
## (emotionality, agreeableness, conscientiousness, and openness to experience)
## and partner selection for a cooperation  task

## a.	To what extent is target personality related to partner selection for a cooperative task?
## b.	To what extent is relational personality related to partner selection for a cooperative task?





#### RESEARCH QUESTION 3 ####

## Moderating role of task type (trust-based versus competence-based)
## on the relationship between target and relational personality (emotionality, agreeableness, conscientiousness, and openness to experience)
## and partner selection for a cooperation decision task

## Is the relation between target and relational personality, and partner selection moderated by task type?





#### RESEARCH QUESTION 4 ####

## Relationship between similarity on HONESTY-HUMILITY and OPENNESS TO EXPERIENCE, and LIKING
## a.	To what extent is similarity on target honesty-humility and openness to experience related to liking?
## b.	To what extent is similarity on relational honesty-humility and openness to experience related to liking?
  




#### RESEARCH QUESTION 5 ####

# Relationship between similarity on HONESTY-HUMILITY and OPENNESS TO EXPERIENCE, and PARTNER SELECTION
# a.	To what extent is similarity on target honesty-humility and openness to experience related to partner selection?
# b.	To what extent is similarity on relational honesty-humility and openness to experience related to partner selection?





#### RESEARCH QUESTION 6 ####

## Relationship between HONESTY-HUMILITY and GIVING in a cooperative trust-based task
## a.	To what extent is target honesty-humility related to giving in a trust-based task?
## b.	To what extent is relational honesty-humility related to giving in a trust-based task?





