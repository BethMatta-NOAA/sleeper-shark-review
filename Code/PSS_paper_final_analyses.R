###Pacific sleeper shark Somniosus pacificus (PSS)
###Code used to create figures in Matta et al. review paper
###Data from multiple sources (available to GOA and BSAI shark assessments)


#LOAD PACKAGES -----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rgdal)
library(nlstools)



# FIGURE 2. Survey index timeseries ---------------------------------------

# ..IPHC Longline------------------------------------------------------------------
IPHC <- read.csv("Data/IPHC_FISS_CPUE98_21.csv")
names(IPHC) <- tolower(names(IPHC))
IPHC<-IPHC[IPHC$area_combo=="FMP (without Inside waters)" & IPHC$species == "Sleeper shark",] #same data as in assessment
IPHC$area <- as.factor(IPHC$area)

#create scaled index and prepare data for combination with other datasets
IPHC <- IPHC %>% 
  group_by(area) %>% 
  mutate(norm_index = scale(area_cpue)) %>% 
  select(survey_year, area, area_cpue, norm_index) %>% 
  rename(index = area_cpue,
         year = survey_year) %>% 
  mutate(source = "IPHC",
         survey = str_c(source," ",area),
         gear = "longline",
         index_type = "area_cpue") %>% 
  as.data.frame()


# ..RACE Trawl------------------------------------------------------------------
RACE <- read.csv("Data/RACE_Biomass_Sharks.csv")
RACE <- RACE[RACE$Group == "Pacific Sleeper Shark",]
RACE <- RACE[RACE$SURVEY == RACE$REGULATORY_AREA_NAME,]

#create scaled index and prepare data for combination with other datasets
RACE <- RACE %>% 
  group_by(SURVEY) %>% 
  mutate(norm_index = scale(Biomass)) %>% 
  select(YEAR, SURVEY, Biomass, norm_index) %>% 
  rename(area = SURVEY,
         year = YEAR,
         index = Biomass) %>% 
  mutate(source = "RACE",
         survey = str_c(source," ",area),
         gear = "trawl",
         index_type = "biomass") %>% 
  as.data.frame()

RACE <- read.csv("Data/RACE_Biomass_Sharks2022.csv")
RACE <- RACE[RACE$Group == "Pacific Sleeper Shark",]
RACE <- RACE[RACE$SURVEY == RACE$REGULATORY_AREA_NAME,]

#create scaled index and prepare data for combination with other datasets
RACE <- RACE %>% 
  group_by(SURVEY) %>% 
  filter(SURVEY != "NBS") %>% 
  mutate(norm_index = scale(Biomass)) %>% 
  select(YEAR, SURVEY, Biomass, norm_index) %>% 
  rename(area = SURVEY,
         year = YEAR,
         index = Biomass) %>% 
  mutate(source = "RACE",
         survey = str_c(source," ",area),
         gear = "trawl",
         index_type = "biomass") %>% 
  as.data.frame()


# ..AFSC Longline ----------------------------------------------------------------------
AFSC_LL <- read.csv("Data/AFSCLL_RPN2021.csv")
AFSC_LL <- AFSC_LL[AFSC_LL$Species.Code==320,]
AFSC_LL[AFSC_LL == 0 & AFSC_LL$FMP=="BSAI" & AFSC_LL$Year !=2017] <- NA  #Replace zero values for non-survey years with NA (need to check these are correct)
AFSC_LL <- na.omit(AFSC_LL)

#create scaled index and prepare data for combination with other datasets
AFSC_LL <- AFSC_LL %>% 
  group_by(FMP) %>% 
  mutate(norm_index = scale(sumRPN)) %>% 
  select(Year, FMP, sumRPN, norm_index) %>% 
  rename(year = Year,
         area = FMP,
         index = sumRPN) %>% 
  mutate(source = "AFSC",
         survey = str_c(source," ",area),
         gear = "longline",
         index_type = "RPN") %>% 
  as.data.frame()


# ..ADFG Longline-----------------------------------------------------------------
#Prince William Sound and Southeast (2 separate surveys)

#Southeast
ADFG_SEAK <- read.csv("Data/ADFG_SEAK_LL2020.csv")
ADFG_SEAK <- ADFG_SEAK %>% 
  group_by(Year) %>% 
  summarise(Tot_hks=sum(hooks_tot),
            ineff_hks=sum(hooks_inval),
            toteffhks=Tot_hks-ineff_hks, 
            PSS=sum(PSS_no),
            CPUE_PSS=PSS/toteffhks) #calculate CPUE (from assessment)
#create scaled index and prepare data for combination with other datasets
ADFG_SEAK <- ADFG_SEAK %>% 
  mutate(norm_index = scale(CPUE_PSS),
         area = "GOA") %>% 
  select(Year, area, CPUE_PSS, norm_index) %>% 
  rename(year = Year,
         index = CPUE_PSS) %>% 
  mutate(source = "ADFG",
         survey = str_c(source," SE AK"),
         gear = "longline",
         index_type = "CPUE") %>% 
  as.data.frame()

#PWS
ADFG_PWS <- read.csv("Data/ADFG_PWS_LLSurvey_CPUE.csv")
ADFG_PWS <- ADFG_PWS[ADFG_PWS$Survey=="Prince William Sound Longline Survey",]
ADFG_PWS <- reshape2::melt(ADFG_PWS[,c("Year","pkEvent_ID","Sleeper_Shark")], id.vars=c("Year","pkEvent_ID"))
ADFG_PWS <- ADFG_PWS %>% 
  group_by(Year) %>% 
  summarise(avgCPUE = mean(value))
#create scaled index and prepare data for combination with other datasets
ADFG_PWS <- ADFG_PWS %>% 
  mutate(norm_index = scale(avgCPUE),
         area = "GOA") %>%
  select(Year, area, avgCPUE, norm_index) %>% 
  rename(year = Year,
         index = avgCPUE) %>% 
  mutate(source = "ADFG",
         survey = str_c(source," PWS"),
         gear = "longline",
         index_type = "CPUE") %>% 
  as.data.frame()


# ..DFO Sablefish Pot  -----------------------------------------------------------------

sablefishtrapsurvey <- readRDS("Data/DFO_Lindsay_Feb2023/SleeperSablefishCPUE_230221.rds") %>%
  group_by(survey_series_id, year) %>%
  summarize(sumtraps = sum(traps_fished_count, na.rm = TRUE),
            sumcatch = sum(catch_count, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(cpue = sumcatch / sumtraps) %>% 
  filter(survey_series_id==41)  #show standardized inlet survey only


#create scaled index and prepare data for combination with other datasets
DFO_POT <- sablefishtrapsurvey %>% 
  mutate(norm_index = scale(cpue),
         area = "CAN") %>%
  select(year, area, cpue, norm_index) %>%
  rename(index = cpue) %>% 
  mutate(source = "DFO",
         survey = str_c(source," BC"),
         gear = "pot",
         index_type = "CPUE") %>% 
  as.data.frame()


# ..Combined survey indices --------------------------------------------------
combo_surveys <- rbind(IPHC,RACE,AFSC_LL,ADFG_SEAK,DFO_POT)

#define broad regions
combo_surveys$region <- ifelse(combo_surveys$area == "GOA", "GOA", 
                               ifelse(combo_surveys$area == "CAN", "BC",
                                      ifelse(combo_surveys$area == "WC", "WC",
                                             "BSAI")))
combo_surveys$region <- factor(combo_surveys$region, 
                               levels=c("BSAI","GOA","BC","WC"))
combo_surveys$survey <- factor(combo_surveys$survey,
                               levels = c("RACE EBS_SHELF", 
                                          "RACE EBS_SLOPE", 
                                          "AFSC BSAI", 
                                          "IPHC BSAI", 
                                          "RACE AI",
                                          "RACE GOA", 
                                          "AFSC GOA", 
                                          "IPHC GOA",
                                          "ADFG SE AK", 
                                          "DFO BC",
                                          "IPHC CAN",
                                          "IPHC WC"),
                               labels = c("NOAA BS Shelf (T)", 
                                          "NOAA BS Slope (T)", 
                                          "NOAA BS&AI (LL)", 
                                          "IPHC BS&AI (LL)", 
                                          "NOAA AI (T)",
                                          "NOAA GOA (T)", 
                                          "NOAA GOA (LL)", 
                                          "IPHC GOA (LL)",
                                          "ADFG SEAK (LL)", 
                                          "DFO BC (P)",
                                          "IPHC BC (LL)",
                                          "IPHC WC (LL)"))
colscombo <- c("NOAA BS Shelf (T)"="#e64b35", 
               "NOAA BS Slope (T)"="#e64b35", 
               "NOAA BS&AI (LL)"="#e64b35", 
               "IPHC BS&AI (LL)"="#e64b35", 
               "NOAA AI (T)"="#4dbbd5",
               "NOAA GOA (T)"="#efc000", 
               "NOAA GOA (LL)"="#efc000", 
               "IPHC GOA (LL)"="#efc000",
               "ADFG SEAK (LL)"="#efc000", 
               "DFO BC (P)"="#00a087",
               "IPHC BC (LL)"="#00a087",
               "IPHC WC (LL)"="#3c5488")

#Survey indices (panel for each survey, not normalized)
combo.survey.plot.index <- ggplot(combo_surveys, aes(year, index, group=survey, color=survey, shape=survey)) +
  facet_wrap(~survey, scales = "free_y") +
  geom_point() +
  geom_line() +
  labs(x="Year", y="Survey index\n") +
  scale_color_manual(values=colscombo,
                     name="Survey",
                     labels=levels(combo_surveys$survey)) +
  scale_shape_manual(values=1:12,
                     name="Survey",
                     labels=levels(combo_surveys$survey)) +
  theme_pubr(border = TRUE,
             legend = "none",
             x.text.angle = 45) +
  theme(axis.text.y = element_blank())

#Manuscript version
ggsave(path = "Figures/Manuscript_version", filename = "Fig2.tiff", 
       plot = combo.survey.plot.index, dpi = 600, units="mm", width = 174, height = 125, bg="transparent")


ggplot(combo_surveys, aes(year, norm_index, color=survey)) +
  facet_grid(region~.) +
  geom_point()+ geom_line()


# FIGURE 3. Fishery map---------------------------------------------------------------

#spatial data from https://www.fisheries.noaa.gov/resource/map/spatial-data-collected-groundfish-observers-alaska
spatialfisherycatch <- read.csv("Data/MOST_FISH_080822.csv") #retrieved August 8, 2022

#omit nonPSS
spatialfisherycatch <- filter(spatialfisherycatch, SPECIES == "Pacific Sleeper Shark")

#rename columns
names(spatialfisherycatch)[names(spatialfisherycatch) == "YEAR"] <- "Year"
names(spatialfisherycatch)[names(spatialfisherycatch) == "LAT400SQKM"] <- "Lat"
names(spatialfisherycatch)[names(spatialfisherycatch) == "LON400SQKM"] <- "Long"

#adjust data points for 180 line
spatialfisherycatch$Long = ifelse(spatialfisherycatch$Long > 0, spatialfisherycatch$Long - 360, spatialfisherycatch$Long)

#get land
world <- ne_countries(scale = "medium", returnclass = "sp")
#usa
usa <- subset(world, admin == "United States of America")
usa <- fortify(usa)
usa$long <- ifelse(usa$long > 0, usa$long - 360, usa$long)
#russia
russia <- subset(world, admin == "Russia")
russia <- fortify(russia)
russia$long = ifelse(russia$long > 0, russia$long - 360, russia$long)
#canada
canada <- subset(world, admin == "Canada")
canada <- fortify(canada)

#bathymetry (from downloaded OFIS bathymetry shapefile)
race_bathy <- readOGR("Data/race_bathy_to_200_NAD1983_HARN")
race_bathy_df <- fortify(race_bathy)
race_bathy_df$long <- ifelse(race_bathy_df$long > 0, race_bathy_df$long - 360, race_bathy_df$long)

#Sleeper shark catch map totals since 1997 
PSScatchmap <- filter(spatialfisherycatch, Year >= 1997) %>%
  group_by(Lat, Long) %>%
  summarize(Kilos_total=sum(KG),
            Kilos_avg=mean(KG))

PSScatchmap <- PSScatchmap %>%
  mutate(Tons_total=Kilos_total/1000,
         Tons_avg=Kilos_avg/1000)

#show total catch per grid cell (1997-2021)
PSS_catchmap_plot <- 
  ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#18191A", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#8c8c8c", color = "#18191A", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#8c8c8c", color = "#18191A", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = PSScatchmap,
             aes(x = Long, y = Lat, color=Tons_total), 
             shape = 15, size=1.5) +
  scale_color_viridis_c(option="inferno", space="Lab", 
                        begin = 0.9, end = 0, 
                        guide = guide_colourbar(direction = "horizontal")) +
  scale_x_continuous(breaks = seq(-185,-135, by = 10)) +
  scale_y_continuous(breaks = seq(45,65, by = 2)) +
  labs(color = "Total catch (metric tons)") +
  coord_map(projection = "albers",lat0=40, lat1=55,
            xlim = c(min(PSScatchmap$Long), max(PSScatchmap$Long)),
            ylim = c(min(PSScatchmap$Lat), max(PSScatchmap$Lat)))+
  theme_bw() +
  theme(legend.position = "top",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(),
        panel.spacing = unit(0.5, "lines"),
        rect = element_rect(fill = "transparent"))

#Manuscript version
ggsave(path = "Figures/Manuscript_version", filename = "Fig3.tiff", 
       plot = PSS_catchmap_plot, dpi = 600, units="mm", width = 174, height = 90, bg="transparent")


# FIGURE 4. Catch by gear Canada---------------------------------------
#Data from Lindsay (DFO)
#Trawl sectors are mostly reported in kg, other sections in pcs (numbers)
commcatch3 <- readRDS("Data/DFO_Linsday_Jan2023/Sleepercommcatch.rds") # commercial catch for trawl, longline
commcatch3$gear <- recode(commcatch3$gear, TRAP = "POT")

comcatch_sum <- commcatch3 %>%
  group_by(year, gear) %>%
  summarise(
    discarded_pcs_yearsum = sum(discarded_pcs),
    landed_pcs_yearsum = sum(landed_pcs),
    discarded_kg_yearsum = sum(discarded_kg),
    landed_kg_yearsum = sum(landed_kg)
  )

canada.catch <- comcatch_sum %>% 
  group_by(year, gear) %>% 
  mutate(tot_catch_kg = sum(discarded_kg_yearsum, landed_kg_yearsum),
         tot_catch_t = tot_catch_kg/1000,
         tot_catch_pcs = sum(discarded_pcs_yearsum, landed_pcs_yearsum),
         gear_cat = ifelse(str_detect(gear, "TRAWL"), "Trawl", "Non-trawl"))
canada.catch$gear <- str_to_title(canada.catch$gear) 

trawl <- ggplot(canada.catch[canada.catch$gear_cat == "Trawl",], 
                aes(year, tot_catch_t, fill=gear)) +
  geom_bar(stat = "identity") +
  facet_wrap(~gear_cat) +
  labs(x = "Year", y = "Catch (metric tons)\n", fill = "Gear Type") +
  scale_x_continuous(breaks = seq(1995,2022, 5)) +
  coord_cartesian(xlim = c(1995,2022)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#999999")) +
  theme_pubr(border = TRUE, legend = "right") +
  theme(axis.title.x = element_blank())

nontrawl <- ggplot(canada.catch[canada.catch$gear_cat == "Non-trawl",], 
                   aes(year, tot_catch_pcs, fill=gear)) +
  geom_bar(stat = "identity") +
  facet_wrap(~gear_cat) +
  labs(x = "Year", y = "Catch (numbers)\n", fill = "Gear Type") +
  scale_x_continuous(breaks = seq(1995,2022, 5)) +
  coord_cartesian(xlim = c(1995,2022)) +
  scale_fill_manual(values = c("#CC6666", "#9999CC")) +
  theme_pubr(border = TRUE, legend = "right") +
  theme(legend.title = element_blank())

canada.catch.plot <- trawl / nontrawl + 
  plot_layout(guides = "collect")

#Manuscript version
ggsave(path = "Figures/Manuscript_version", filename = "Fig4.tiff", 
       plot = canada.catch.plot, dpi = 600, units="mm", width = 174, height = 125, bg="transparent")



# FIGURE 5. Catch by target fishery AK---------------------------------------
#Data contains confidential information that cannot be shared

#Load/prep data (updated 8/8/2022)
fisherycatch <- read.csv("Data/confidential_CAS_GFTBF_Sharks2022.csv")
#omit nonPSS
fisherycatch <- filter(fisherycatch, Species == "\"Pacific sleeper shark\"")

fisherycatch <- fisherycatch %>% 
  rename(Year = ï..Year) %>% 
  filter(Year <= 2021) #limit to 2003-2021 because 2022 doesn't represent a full year

#rename gear levels
fisherycatch$Gear <- as.factor(fisherycatch$Gear)
levels(fisherycatch$Gear)
levels(fisherycatch$Gear) <- list(Longline="HAL", "Non-pelagic trawl"="NPT", Pot="POT", "Pelagic trawl" = "PTR")

#change target to factors
fisherycatch$Trip.Target.Group <- as.factor(fisherycatch$Trip.Target.Group)
levels(fisherycatch$Trip.Target.Group)
#group Kamchatka with flatfish
fisherycatch$Trip.Target.Group <- recode_factor(fisherycatch$Trip.Target.Group, 
                                                "Kamchatka Flounder - BSAI" = "Flatfish")
#rename halibut to Pacific halibut
fisherycatch$Trip.Target.Group <-
  plyr::revalue(fisherycatch$Trip.Target.Group, c("Halibut" = "Pacific Halibut"))

#rename areas
fisherycatch$FMP.Area <- as.factor(fisherycatch$FMP.Area)
levels(fisherycatch$FMP.Area)
levels(fisherycatch$FMP.Area) <- 
  list("BSAI"="BSAI", "GOA"="GOA", "Inside"="INSD")


#Catch timeseries by target
#lollipop chart (alternative to donut)
lollipop.target <- fisherycatch %>% 
  group_by(Trip.Target.Group) %>% 
  summarise(Wt_percent = round(sum(Catch..mt.)/sum(fisherycatch$Catch..mt.)*100,0)) %>% 
  mutate(Trip.Target.Group = fct_reorder(Trip.Target.Group, Wt_percent)) %>% 
  ggplot(aes(Trip.Target.Group, Wt_percent)) +
  geom_segment(aes(xend=Trip.Target.Group, yend=0), size=1) +
  geom_point(size=4, color="orange") +
  coord_flip() +
  labs(x="", y="Percentage of total catch") +
  theme_bw() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

#barplot by area
catch_gear_target <- ggplot(fisherycatch, aes(Year, Catch..mt., fill=Trip.Target.Group)) +
  geom_bar(stat = "identity") +
  facet_grid(FMP.Area~.) +
  labs(x = "Year", y = "Catch (metric tons)", fill = "Target Species") +
  scale_x_continuous(breaks = seq(1990,2020, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "BrBG") +
  theme_pubr(border = TRUE, legend = "right") +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = -10),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

combo.plot.target <- 
  lollipop.target/catch_gear_target +
  plot_layout(heights = unit(c(1.5,7), c("in", 'null'))) +
  plot_annotation(tag_levels = 'a', 
                  theme = theme(axis.title = element_text(size = 8)))

ggsave(path = "Figures", filename = "combo catch target.png", 
       plot = combo.plot.target, dpi = 600, height=8.5)
#Manuscript version
ggsave(path = "Figures/Manuscript_version", filename = "Fig5.tiff", 
       plot = combo.plot.target, dpi = 600, units="mm", width=174, height=210, bg="transparent")


# FIGURE 6. Length conversions --------------------------------------------

#Data from Cindy
#includes more info on estimated and measured metrics (fewer records than full length dataset)
length2 <- read.csv("Data/S_pacificus_size_summary.csv")

#Prepare data
#reduce dataset to Hulbert paper (only fish where PCL, FL, and TL were all measured)
lengthconv <- filter(length2, PCL_type == "measure" & FL_type == "measure" & TL_type == "measure")
#work in cm (TL already in cm, convert PCL and FL)
lengthconv$PCLcm <- lengthconv$PCL/10
lengthconv$FLcm <- lengthconv$FL/10

#Linear regression results and graphs
#Convert PCL to TL
TL_PCL <- lm(TLcm ~ PCLcm, data = lengthconv)
summary(TL_PCL)
sigma(TL_PCL)*100/mean(lengthconv$PCLcm) #prediction error rate

#make label for graph
matrix_coef1 <- summary(TL_PCL)$coefficients
int1 <- round(matrix_coef1[1,1],2)
slope1 <- round(matrix_coef1[2,1],2)
r2.1 <- round(summary(TL_PCL)$r.squared, 3)
mylabel1 <- bquote(italic(TL) == .(int1) + .(slope1) ~ italic(PCL) ~ "," ~
                     italic(R^2) == .(r2.1))

TL_PCL_lm <- ggplot(lengthconv, aes(PCLcm,TLcm)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, color = "black", fill="blue") +
  annotate("text", x=-Inf, y=Inf, parse= FALSE, 
           label=mylabel1,
           hjust=-0.05, vjust=2, size=3)+
  labs(x = "\nPrecaudal length (cm)", y = "Total length (cm)\n") +
  theme_pubr(base_size = 10)

#Convert FL to TL
TL_FL <- lm(TLcm ~ FLcm, data = lengthconv)
summary(TL_FL)
sigma(TL_FL)*100/mean(lengthconv$FLcm, na.rm = TRUE) #prediction error rate

#make label for graph
matrix_coef2 <- summary(TL_FL)$coefficients
int2 <- round(matrix_coef2[1,1],2)
slope2 <- round(matrix_coef2[2,1],2)
r2.2 <- round(summary(TL_FL)$r.squared, 3)
mylabel2 <- bquote(italic(TL) == .(int2) + .(slope2) ~ italic(FL) ~ "," ~
                     italic(R^2) == .(r2.2))

TL_FL_lm <- ggplot(lengthconv, aes(FLcm,TLcm)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, color = "black", fill="blue") +
  annotate("text", x=-Inf, y=Inf, parse= FALSE, 
           label=mylabel2,
           hjust=-0.05, vjust=2, size=3)+
  labs(x = "\nFork length (cm)", y = "Total length (cm)\n") +
  theme_pubr(base_size = 10)

#combine graphs into one figure
length_reg <- TL_PCL_lm / TL_FL_lm + 
  plot_annotation(tag_levels = 'a')

#Manuscript version
ggsave(path = "Figures/Manuscript_version", filename = "Fig6.tiff", 
       plot = length_reg, dpi = 600, units="mm", width = 84, bg="transparent")


# FIGURE 7. Males vs females ----------------------------------------------
#From stock assessments
#Updated in 2020

#Read and prepare length data
PSSlengths <- read.csv("Data/Sleeper_lengths_coast2020.csv")
PSSlengths$FMP<-factor(PSSlengths$FMP,levels=c("BS","AI","GOA","BC","WC"))
colsarea<-c("BS"="#e64b35", "AI"="#4dbbd5", "GOA"="#efc000", "BC"="#00a087", "WC"="#3c5488")

#rename gear categories
PSSlengths <- PSSlengths %>%
  mutate(Gear2 =
           ifelse(Gear %in% c("", "Unknown"), "Unknown",
                  ifelse(Gear %in% c("Bottom Trawl", "Trawl"), "Trawl",
                         ifelse(Gear %in% c("Longline", "V LL"), "Longline", 
                                ifelse(Gear %in% c("Stranding"), "Stranding",
                                       "other")))))
PSSlengths$Gear2 <- factor(PSSlengths$Gear2)

#rename sex categories
PSSlengths$Sex <- factor(PSSlengths$Sex)
levels(PSSlengths$Sex) <- list("Male"="1", "Female"="2", "Unknown"="3", "Unknown"="", "Unknown"="1 or 2")

#remove NA lengths
PSSlengths <- PSSlengths %>% drop_na(TLcm)
#expand the frequencies out
PSSlengths <-PSSlengths[rep(1:nrow(PSSlengths), PSSlengths[["Frequency"]]), ]


# ..Length distributions by sex -------------------------------------------
#Count numbers of each sex
PSSlengths %>%
  group_by(Sex) %>%
  summarize(count=n())

lengthsex <- ggplot(filter(PSSlengths, Sex!="Unknown"), aes(x=TLcm, fill=Sex)) +
  geom_density(color="black", alpha=0.3) +
  labs(x="Total Length (cm)", y="Density") +
  scale_fill_manual(values = c("#0073cf", "#b8023e"),
                    labels = c("Male (n=637)", "Female (n=750)")) +
  theme_pubr(base_size = 10) +
  theme(legend.position=c(0.8,0.8),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2))

#KS Test to compare male and female length distributions
ks.test(PSSlengths$TLcm[PSSlengths$Sex == "Male"],
        PSSlengths$TLcm[PSSlengths$Sex == "Female"])

#..Sex ratio by FMP--------------------------------
sexratio <- filter(PSSlengths, Sex!="Unknown") %>%
  group_by(FMP,Sex) %>%
  summarize(count=n()) %>%
  mutate(freq = count/sum(count))

#piechart
piechart_sex_FMP <- ggplot(data = sexratio, aes(x = "", y = freq, fill = Sex)) + 
  geom_bar(stat = "identity", color = "black", alpha=0.3) + 
  coord_polar("y") +
  facet_grid(~FMP, switch = "x") +
  scale_fill_manual(values = c("#0073cf", "#b8023e")) + 
  geom_text(aes(label = paste0("n=",count)),
            position = position_stack(vjust = 0.5),
            size=3) +
  theme_void(base_size = 10) +
  theme(legend.position = "none",
        strip.text = element_text(size=12))


# ..Combo plot Fig. 7 -----------------------------------------------------
#Add sex ratio piechart to length density plot
layout <- c(
  area(t = 1, l = 1, b = 6, r = 5),
  area(t = 7, l = 1, b = 9, r = 5)
)

combo_ratio_density <- 
  lengthsex / piechart_sex_FMP + plot_annotation(tag_levels = 'a') +  
  plot_layout(design = layout)

#Manuscript version
ggsave(path = "Figures/Manuscript_version", filename = "Fig7.tiff", 
       plot = combo_ratio_density, dpi = 600, units="mm", width = 174, height = 100, bg="transparent")


# FIGURE 8. Length distributions by region --------------------------------

#Calculate mean size by region
meanTLcm.area <- PSSlengths %>% 
  group_by(FMP) %>% 
  summarise(mean = mean(TLcm),
            count = n())

#Histograms with mean size, length by region
PSS.lengths.histo <- ggplot(PSSlengths, aes(x=TLcm, fill=FMP)) +
  geom_histogram(color="black") +
  facet_grid(FMP~., scales="free_y") +
  geom_vline(data = meanTLcm.area, aes(xintercept = mean), 
             linetype = "dotted", size = 2) +
  geom_vline(xintercept = 370, #estimated size at maturity from Ebert et al. (1987)
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = colsarea) +
  labs(x="Total Length (cm)", y="Number of sharks\n") +
  theme_pubr(base_size = 12, legend="none", border=TRUE) +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2))

#Manuscript version
ggsave(path = "Figures/Manuscript_version", filename = "Fig8.tiff", 
       plot = PSS.lengths.histo, dpi = 600, units="mm", width = 174, height = 174, bg="transparent")


# FIGURE 9. Length-weight relationship ------------------------------------

#subset the size summary file, measured weights only and get rid of TL NA values
Wt_meas <- subset(length2, Weight_type == "measure")
Wt_meas <- Wt_meas %>% tidyr::drop_na(TLcm)

#Load NORPAC data (from Cindy, email dated 6/16/2020) and prep for joining with size summary file
NORPAC_LW <- read.csv("Data/Somniosus_LW_NORPAC.csv")
NORPAC_LW$Source <- "NORPAC"
colnames(NORPAC_LW) <- str_to_title(colnames(NORPAC_LW))
NORPAC_LW <- NORPAC_LW %>% 
  rename(TLcm = Length)
NORPAC_LW$Sex <- recode(NORPAC_LW$Sex, M=1, F=2, U=3)

#join data files
Wt_meas <- full_join(Wt_meas, NORPAC_LW)


# ..Figure 9a panel -------------------------------------------------------

#Do linear fits (ln transform length and weight)
#Remove two obvious outliers
#40 cm individual on RACE trawl survey recorded as weighing 27.398 kg
#93 cm individual on RACE trawl survey recorded as weight 62 kg
Wt_meas <- Wt_meas[-c(226,40),]

#Fit separately by sex
unique(Wt_meas$Sex) #get unique sex codes
table(Wt_meas$Sex) #count number of each sex
Wt_meas_sex <- Wt_meas[Wt_meas$Sex != 3,] #filter out unknown sex 3

lnLW.sex <- ggplot(Wt_meas_sex, aes(log(TLcm), log(Weight), color=factor(Sex))) +
  geom_point(alpha=0.3, size=2) +
  geom_smooth(method="lm",se=FALSE) +
  labs(x="ln Total length (cm)", y="ln Weight (kg)", color = "") +
  scale_color_manual(values = c("#0073cf", "#b8023e"), labels = c("Male", "Female")) +
  theme_pubr(base_size = 11) +
  theme(legend.position = c(0.2,0.9),
        legend.background = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

lnLW.fit.sex <- lm(log(Weight)~log(TLcm)*Sex, data=Wt_meas_sex)
summary(lnLW.fit.sex) #sex not significant

#Sexes combined (includes 77 sharks that were of unknown sex)
lnLW.fit <- lm(log(Weight)~log(TLcm), data=Wt_meas)
summary(lnLW.fit)
confint(lnLW.fit)
matrix_coef <- summary(lnLW.fit)$coefficients
our_data <- function(x){exp(matrix_coef[1,1]) * (x^matrix_coef[2,1])} #transform a parameter back to kg


# ..Figure 9b panel -------------------------------------------------------

#create functions to overlay known PSS curves
#if units for weight were grams in their analysis, I divided "a" by 1000 to change to kg
PSS_Fishbase <- function(x) {0.00000661 * (x^3.08)}
PSS_Orlov2014 <- function(x) {0.0000603 * (x^2.695)}#Published in Orlov & Baitalyuk (2014), based on n=905, length range = ca. 50-530 cm TL
PSS_Yano <- function(x) {0.000004257 * (x^3.135)}#Published in Yano et al. (2007), based on n=26, length range=41.8-430.0 cm TL

LW.curves <- ggplot(Wt_meas, aes(x=TLcm, y=Weight))+
  stat_function(fun=our_data, linetype="solid", color="black", size=1.2) +
  stat_function(fun=PSS_Fishbase, linetype="dashed", color="#2f93c4", size=1) +
  stat_function(fun=PSS_Orlov2014, linetype="dotdash", color="#32a852", size=1) +
  stat_function(fun=PSS_Yano, linetype="longdash", color="#c42f52", size=1) +
  labs(x="Total length (cm)", y= "Weight (kg)") +
  coord_cartesian(ylim=c(0,1000)) +
  annotate("text", x=20, y=1000, parse= TRUE, 
           label= as.character(expression(paste("W = ",7.65,e^"-06 ", TL ^ "3.043", "   This paper"))), 
           hjust=0, size=3)+
  annotate("text", x=20, y=920, parse= TRUE, 
           label= as.character(expression(paste("W = ",6.61,e^"-06 ", TL ^ "3.080", "   Fishbase"))), 
           hjust=0, size=3, color="#2f93c4")+
  annotate("text", x=20, y=840, parse= TRUE, 
           label= as.character(expression(paste("W = ",6.03,e^"-05 ", TL ^ "2.695", "   Orlov & Baitalyuk 2014"))), 
           hjust=0, size=3, color="#32a852")+
  annotate("text", x=20, y=760, parse= TRUE, 
           label= as.character(expression(paste("W = ",4.26,e^"-06 ", TL ^ "3.135", "   Yano et al. 2007"))), 
           hjust=0, size=3, color="#c42f52")+
  theme_pubr(legend="none", base_size = 11) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

#combine graphs into one figure
LW.relps <- lnLW.sex / LW.curves + 
  plot_annotation(tag_levels = 'a')

#Manuscript version
ggsave(path = "Figures/Manuscript_version", filename = "Fig9.tiff", 
       plot = LW.relps, dpi = 600, units="mm", width = 84, height = 150, bg="transparent")

