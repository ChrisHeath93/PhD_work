library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(afex) #load afex for running factorial ANOVA
library(emmeans) #load emmeans for running pairwise comparisons
library(ggforce)
library(ica)



#Load data from excel, map to 'data' name

data <- read_excel("filepath here", col_names = TRUE, col_types = NULL, trim_ws = TRUE)

# Depending on order of approaches, treatments occur between different assessment points. 
# I am splitting the participants into their approach orders, creating a column for the correct assessment
# points to measure change over treatment, and recombining to create an improvement over
# the course of treatment column
#FU = Followup (Long-term imrprovement/change)
Order1 <- filter(data, Order == 1)
Order2 <- filter(data, Order == 2)
Order3 <- filter(data, Order == 3)

Order1 <- mutate(Order1, GestImp = GestInt2 - GestInt1,
                 GestFU = GestPost - GestInt1,
  ProsImp = ProsInt2 - ProsInt1, 
  ProsFU = ProsPost - ProsInt1,
  PhonImp = PhonInt1 - PhonPre,
  PhonFU = PhonInt2 - PhonPre,
  SemImp = SemInt1 - SemPre,
  SemFU = SemInt2 - SemPre,
  SpeImp = SpePost - SpeInt2,
  SpeFU = SpeFollowup - SpeInt2,
  IntImp = IntPost - IntInt2,
  IntFU = IntFollowup - IntInt2,
  Ctrl1Imp = CtrlInt1 - CtrlPre,
  Ctrl2Imp = CtrlInt2 - CtrlInt1,
  Ctrl3Imp = CtrlPost - CtrlInt2,
  Pros_PreNew = ProsInt1,
  Pros_PostNew = ProsInt2,
  Phon_PreNew = PhonPre,
  Phon_PostNew = PhonInt1,
  Sem_PreNew = SemPre,
  Sem_PostNew = SemInt1,
  Spe_PreNew = SpeInt2,
  Spe_PostNew = SpePost,
  Int_PreNew = IntInt2,
  Int_PostNew = IntPost,
  Gest_PreNew = GestInt1,
  Gest_PostNew = GestInt2,
  Ctrl1_PreNew = CtrlPre,
  Ctrl1_PostNew = CtrlInt1,
  Ctrl2_PreNew = CtrlInt1,
  Ctrl2_PostNew = CtrlInt2,
  Ctrl3_PreNew = CtrlInt2,
  Ctrl3_PostNew = CtrlPost
)

Order2 <- mutate(Order2, GestImp = GestInt1 - GestPre,
                 GestFU = GestInt2 - GestPre,
  ProsImp = ProsInt1 - ProsPre,
  ProsFU = ProsInt2 - ProsPre,
  PhonImp = PhonPost - PhonInt2,
  PhonFU = PhonFollowup - PhonInt2,
  SemImp = SemPost - SemInt2,
  SemFU = SemFollowup - SemInt2,
  SpeImp = SpeInt2 - SpeInt1,
  SpeFU = SpePost - SpeInt1,
  IntImp = IntInt2 - IntInt1,
  IntFU = IntPost - IntInt1,
  Ctrl1Imp = CtrlPost - CtrlInt2,
  Ctrl2Imp = CtrlInt1 - CtrlPre,
  Ctrl3Imp = CtrlInt2 - CtrlInt1,
  Pros_PreNew = ProsPre,
  Pros_PostNew = ProsInt1,
  Phon_PreNew = PhonInt2,
  Phon_PostNew = PhonPost,
  Sem_PreNew = SemInt2,
  Sem_PostNew = SemPost,
  Spe_PreNew = SpeInt1,
  Spe_PostNew = SpeInt2,
  Int_PreNew = IntInt1,
  Int_PostNew = IntInt2,
  Gest_PreNew = GestPre,
  Gest_PostNew = GestInt1,
  Ctrl1_PreNew = CtrlInt2,
  Ctrl1_PostNew = CtrlPost,
  Ctrl2_PreNew = CtrlPre,
  Ctrl2_PostNew = CtrlInt1,
  Ctrl3_PreNew = CtrlInt1,
  Ctrl3_PostNew = CtrlInt2
)

Order3 <- mutate(Order3, GestImp = GestPost - GestInt2,
                 GestFU = GestFollowup - GestInt2,
  ProsImp = ProsPost - ProsInt2,
  ProsFU = ProsFollowup - ProsInt2,
  PhonImp = PhonInt2 - PhonInt1,
  PhonFU = PhonPost - PhonInt1,
  SemImp = SemInt2 - SemInt1,
  SemFU = SemPost - SemInt1,
  SpeImp = SpeInt1 - SpePre,
  SpeFU = SpeInt2 - SpePre,
  IntImp = IntInt1 - IntPre,
  IntFU = IntInt2 - IntPre,
  Ctrl1Imp = CtrlInt2 - CtrlInt1,
  Ctrl2Imp = CtrlPost - CtrlInt2,
  Ctrl3Imp = CtrlInt1 - CtrlPre,
  Pros_PreNew = ProsInt2,
  Pros_PostNew = ProsPost,
  Phon_PreNew = PhonInt1,
  Phon_PostNew = PhonInt2,
  Sem_PreNew = SemInt1,
  Sem_PostNew = SemInt2,
  Spe_PreNew = SpePre,
  Spe_PostNew = SpeInt1,
  Int_PreNew = IntPre,
  Int_PostNew = IntInt1,
  Gest_PreNew = GestInt2,
  Gest_PostNew = GestPost,
  Ctrl1_PreNew = CtrlInt1,
  Ctrl1_PostNew = CtrlInt2,
  Ctrl2_PreNew = CtrlInt2,
  Ctrl2_PostNew = CtrlPost,
  Ctrl3_PreNew = CtrlPre,
  Ctrl3_PostNew = CtrlInt1
)
         
data_new <- bind_rows(Order1, Order2, Order3)

#Add in overall pre-treatment, post-treatment, and score change values
data_new <- mutate(data_new,
                   PreTotal = Gest_PreNew + Pros_PreNew + Sem_PreNew + Phon_PreNew + Spe_PreNew + Int_PreNew + Ctrl1_PreNew + Ctrl2_PreNew + Ctrl3_PreNew,
                   PostTotal = Gest_PostNew + Pros_PostNew + Sem_PostNew + Phon_PostNew + Spe_PostNew + Int_PostNew + Ctrl1_PostNew + Ctrl2_PostNew + Ctrl3_PostNew,
                   TotalScoreChange = PostTotal - PreTotal
                   )
#filter out participants with incomplete data
data_new <- filter (data_new, Patient != "Pwo")



#change data to long form - includes both values for improvement in each treatment and pre and post score values
data_long <- gather(data_new, "treatment", "Improvement", c("PhonImp", "SemImp", "ProsImp", "GestImp", "SpeImp", "IntImp", "Ctrl1Imp", "Ctrl2Imp", "Ctrl3Imp"))
data_long <- gather(data_long, key = "Timepoint", value = "Score", c("Gest_PreNew", "Pros_PreNew", "Sem_PreNew", "Phon_PreNew", "Spe_PreNew", "Int_PreNew", "Ctrl1_PreNew", "Ctrl2_PreNew", "Ctrl3_PreNew", "Gest_PostNew", "Pros_PostNew", "Sem_PostNew", "Phon_PostNew", "Spe_PostNew", "Int_PostNew", "Ctrl1_PostNew", "Ctrl2_PostNew", "Ctrl3_PostNew"))
                                                

  

# Change names for histogram labels
Histogram1 <- data_long %>%
  mutate(treatment = recode (treatment, 
                            "PhonImp" = "Phonological",
                            "SemImp" = "Semantic",
                            "GestImp" = "Gesture",
                            "ProsImp" = "Prosody",
                            "SpeImp" = "Speeded",
                            "IntImp" = "Interfered",
                            "Ctrl1Imp" = "Model Based",
                            "Ctrl2Imp" = "Alternate Modality",
                            "Ctrl3Imp" = "Executive Function")) 
     


# Histogram for all treated item improvements, charted against participant severity (initial naming score)
Histogram2 <- filter(Histogram1, treatment == "Phonological" | treatment == "Semantic" | treatment == "Gesture" | treatment == "Prosody" | treatment == "Speeded" | treatment == "Interfered") %>%
  select(Startscore, TotalScoreChange, treatment, Improvement) %>%
  distinct()

ggplot(Histogram2, aes(x = Startscore, y = Improvement, colour = treatment)) +
    geom_jitter(width = .1, size = 3, alpha = .75) +
  labs(title = "Naming Improvement by treatment",
       x = "Initial total patient naming score",
       y = "Improvement in treated items naming score") +
  scale_colour_few()
 
#Violin plot comparing improvement in each treatment
ggplot(Histogram2, aes(x = treatment, y = Improvement, fill = treatment)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .2) + 
  stat_summary(fun.data = mean_cl_boot) + 
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plotting data for followup improvement
data_FU <- gather(data_new, "treatment", "Improvement", c("PhonFU", "SemFU", "ProsFU", "GestFU", "SpeFU", "IntFU"))
HistogramFU <- data_FU %>%
  mutate(treatment = recode (treatment, 
                             "PhonFU" = "Phonological",
                             "SemFU" = "Semantic",
                             "GestFU" = "Gesture",
                             "ProsFU" = "Prosody",
                             "SpeFU" = "Speeded",
                             "IntFU" = "Interfered"
                             ))

HistogramFU <- filter(HistogramFU, treatment == "Phonological" | treatment == "Semantic" | treatment == "Gesture" | treatment == "Prosody" | treatment == "Speeded" | treatment == "Interfered") %>%
  select(Startscore, TotalScoreChange, treatment, Improvement) %>%
  distinct()

#Same visualisations for followup data
ggplot(HistogramFU, aes(x = Startscore, y = Improvement, colour = treatment)) +
  geom_jitter(width = .1, size = 3, alpha = .75) +
  labs(title = "Naming Improvement by treatment",
       x = "Initial total patient naming score",
       y = "Improvement in treated items naming score") +
  theme_economist() +
  scale_colour_few()


ggplot(HistogramFU, aes(x = treatment, y = Improvement, fill = treatment)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .2) + 
  stat_summary(fun.data = mean_cl_boot) + 
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Alluvial plot to look at participant improvement consistency between treatments- are the highly improved participants the same on each treatment?

Alluvial <- data_new %>% 
  mutate(MOImp= SemImp + PhonImp) %>%
  mutate(AMImp = GestImp + ProsImp) %>%
  mutate(EDImp = SpeImp + IntImp) %>%
  mutate(Model_Oriented = cut(MOImp, breaks = 20 * -20:60, labels= FALSE)) %>%
  mutate(Alternative_Modality = cut(AMImp, breaks = 20 * -20:60, labels= FALSE)) %>%
  mutate(Executive_Demand = cut(EDImp, breaks = 20 * -20:60, labels=FALSE)) %>%
  mutate(Semantic = cut(SemImp, breaks = 10 * -10:30, labels= FALSE)) %>%
  mutate(Phonological = cut(PhonImp, breaks = 10 * -10:30, labels= FALSE)) %>%
  mutate(Speeded = cut(SpeImp, breaks = 10 * -10:30, labels= FALSE)) %>%
  mutate(Interfered = cut(IntImp, breaks = 10 * -10:30, labels= FALSE)) %>%
  mutate(Gesture = cut(GestImp, breaks = 10 * -10:30, labels= FALSE)) %>%
  mutate(Prosody = cut(ProsImp, breaks = 10 * -10:30, labels= FALSE)) %>%
  select(Patient, Prosody, Semantic, Speeded, Interfered, Gesture, Phonological, Model_Oriented, Alternative_Modality, Executive_Demand)


Alluvial$Model_Oriented <- as.factor(Alluvial$Model_Oriented)
Alluvial$Alternative_Modality <- as.factor(Alluvial$Alternative_Modality)
Alluvial$Executive_Demand <- as.factor(Alluvial$Executive_Demand)
Alluvial$Semantic <- as.factor(Alluvial$Semantic)
Alluvial$Phonological <- as.factor(Alluvial$Phonological)
Alluvial$Speeded <- as.factor(Alluvial$Speeded)
Alluvial$Interfered <- as.factor(Alluvial$Interfered)
Alluvial$Gesture <- as.factor(Alluvial$Gesture)
Alluvial$Prosody <- as.factor(Alluvial$Prosody)



levels(Alluvial$Model_Oriented) <- c("Small Improvement", "Medium Improvement", "Large Improvement")
levels(Alluvial$Alternative_Modality) <- c("No Improvement", "Small Improvement", "Medium Improvement", "Large Improvement")
levels(Alluvial$Executive_Demand) <- c("Small Improvement", "Medium Improvement", "Large Improvement")
levels(Alluvial$Semantic) <- c("Small Improvement", "Medium Improvement", "Large Improvement")
levels(Alluvial$Phonological) <- c("Small Improvement", "Medium Improvement", "Large Improvement")
levels(Alluvial$Speeded) <- c("Small Improvement", "Medium Improvement", "Large Improvement", "Very Large Improvement")
levels(Alluvial$Prosody) <- c("No Improvement", "Small Improvement", "Medium Improvement", "Large Improvement")
levels(Alluvial$Gesture) <- c("No Improvement", "Small Improvement", "Medium Improvement", "Large Improvement")
levels(Alluvial$Interfered) <- c("No Improvement", "Small Improvement", "Medium Improvement", "Large Improvement")


#Participant differences between treatments
Alluvial %>%
  gather_set_data(2:7) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1)) +
  ggtitle("Participant differences by treatment") +
  geom_parallel_sets(aes(fill = Patient), show.legend = FALSE, alpha = 0.3) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0, size = 3) +
  scale_x_discrete(labels = c("Gesture", "Interfered", "Phonological", "Prosody", "Semantic", "Speeded"))


#Participant differences between approaches
Alluvial %>%
  gather_set_data(8:10) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1)) +
  ggtitle("Participant differences by approach") +
  geom_parallel_sets(aes(fill = Patient), show.legend = FALSE, alpha = 0.3) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0, size = 3) +
  scale_x_discrete(labels = c("Alternative Modality", "Executive Demand", "Model Oriented"))



# Histogram for control item changes by approach, charted against participant severity (initial naming score)
Ctrlchangehistogram <- filter(Histogram1, treatment == "Model Based" | treatment == "Alternate Modality" | treatment == "Executive Function") %>%
  select(Startscore, TotalScoreChange, treatment, Improvement) %>%
  distinct()

ggplot(Ctrlchangehistogram, aes(x = Startscore, y = Improvement, colour = treatment)) +
  geom_jitter(width = .1, size = 3, alpha = .75) +
  labs(title = "Control item naming change by approach",
       x = "Initial total patient naming score",
       y = "Change in control items naming score") +
  theme_economist() +
  scale_colour_few()

#Violin plot for control item improvement/change during each approach
ggplot(Ctrlchangehistogram, aes(x = treatment, y = Improvement, fill = treatment)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .2) + 
  stat_summary(fun.data = mean_cl_boot) + 
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Stats

#Research Questions:
#Q1.1: Do all the treatments result in significant improvement in ability to name the treated items?

#paired samples t tests to ensure a significant difference between pre and post treatment scores for naming treated items in all treatments

Phon <- data_new %>%
  select(Phon_PreNew, Phon_PostNew) %>%
  gather()
t.test(value ~ key, data = Phon, paired = TRUE)

Sem <- data_new %>%
  select(Sem_PreNew, Sem_PostNew) %>%
  gather()
t.test(value ~ key, data = Sem, paired = TRUE)

Pros <- data_new %>%
  select(Pros_PreNew, Pros_PostNew) %>%
  gather()
t.test(value ~ key, data = Pros, paired = TRUE)

Gest <- data_new %>%
  select(Gest_PreNew, Gest_PostNew) %>%
  gather()
t.test(value ~ key, data = Gest, paired = TRUE)

Int <- data_new %>%
  select(Int_PreNew, Int_PostNew) %>%
  gather()
t.test(value ~ key, data = Int, paired = TRUE)

Spe <- data_new %>%
  select(Spe_PreNew, Spe_PostNew) %>%
  gather()
t.test(value ~ key, data = Spe, paired = TRUE)



#ANOVA to check with Bonferroni correction







#Q1.2: Do any treatments significantly differ in the improvement to their trained item scores between pre-post therapy? Which treatments are significantly different?
#ANOVA of improvement on each treatment, as well as  

testdata <- filter(Histogram1, treatment == "Phonological" | treatment == "Semantic" | treatment == "Gesture" | treatment == "Prosody" | treatment == "Speeded" | treatment == "Interfered") %>%
  select(Startscore, TotalScoreChange, treatment, Improvement, Patient, Score, Timepoint, Order) %>%
  distinct()

#Remove redundant columns and patients with incomplete data
my_data <- testdata %>%
  select (Patient, Improvement, treatment, Score, Timepoint, Startscore, Order) %>%
  drop_na %>%
  filter (Patient !=  "Pwo") %>%
  mutate(treatment = factor(treatment)) %>%
  mutate(Startscore = factor(Startscore)) %>%
  distinct()

result <- shapiro.test(my_data$Score)


model <- aov_4(Improvement ~ treatment (1 + treatment | Patient), data = my_data)
summary(model)
anova(model)

emmeans(model, pairwise ~ treatment, adjust = "Bonferroni")

#Q1.3: Are these effects maintained over a longer period of time post-treatment?




#Q5: Investigating effects of inter-participant variables on treatment success
my_data <- separate(my_data, Timepoint, c("PrePostTreatment", "PrePost"),  sep = "_") %>%
  select(Patient, Score, PrePost, PrePostTreatment, Startscore, Order) %>%
  distinct() %>%
  spread(PrePost, Score)
my_data <- filter(my_data, PrePostTreatment == "Phon" | PrePostTreatment == "Sem" | PrePostTreatment == "Pros" | PrePostTreatment == "Gest" | PrePostTreatment == "Spe" | PrePostTreatment == "Int")


#ANCOVA using starting naming score for each treatment as a covariate
my_data$PreNew <- scale(my_data$PreNew)
model1 <- aov_4(PostNew ~ PreNew + PrePostTreatment (1 | Patient), data = my_data, factorize = FALSE)
summary(model1)
anova(model1)
emmeans(model1, pairwise ~ PrePostTreatment, adjust = "Bonferroni")


# Next steps - investigate effects of Order and initial overall naming ability (as judged by Startscore)?







#Friedman test for differences in order effects (is this the correct test?)
Ordereffects <- data_new %>%
  select(ProsImp, GestImp, PhonImp, SemImp, SpeImp, IntImp, Order)
  
Ordereffects <-gather(Ordereffects, key = "Timepoint", value ="Improvement", c("PhonImp", "SemImp", "ProsImp", "GestImp", "SpeImp", "IntImp", "GestImp", "ProsImp"))
Ordereffects <- Ordereffects %>%
  group_by_at(vars(-Improvement)) %>%
  mutate(row_id=1:n()) %>%
  spread(key = Order, value = Improvement) %>%
  select(-row_id) %>%
  rename(
    One = 2,
    Two = 3,
    Three = 4)

Ordereffecttable <- matrix(c(Ordereffects$One, Ordereffects$Two, Ordereffects$Three), ncol = 3)

friedman.test(Ordereffecttable)



