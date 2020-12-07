###Ann Wozniak
###Final Project
###12/6/2020

### Load packages
install.packages("ggplot2"); require("ggplot2"); library(ggplot2)
install.packages("vegan"); require("vegan"); library(vegan)
install.packages("lme4"); require("lme4"); library(lme4)
install.packages("effects"); require("effects"); library(effects)
install.packages("gridExtra"); require("gridExtra"); library(gridExtra)
install.packages("plotly"); require("plotly"); library("plotly")
install.packages("shiny"); require("shiny"); library("shiny")
install.packages("foreign"); require("foreign"); library("foreign")

### Set your working directory
setwd("C:/Users/annwozniak/Desktop/EEB_Final_Project_Wozniak")

### Import species and site data
datAPW <- read.csv("Raw_Data/Data_Site_by_Species_Count_v6.csv",head=T,row.names=1, check.names = F)
summary(rowSums(datAPW))
summary(colSums(datAPW)) 

### Order data
datAPW <- datAPW[order(row.names(datAPW)),] # order sites alphabetically
datAPW <- datAPW[,order(colnames(datAPW))] # order species alphabetically

### Import metadata, match up site row names with metadata
mdAPW <- read.csv("Raw_Data/Meta_Data_v6.csv", head=T, row.names=1)
row.names(datAPW) == row.names(mdAPW)

#### Convert metadata
mdAPW$Site <- as.factor(mdAPW$Country)
mdAPW$Domicile_type <- as.factor(mdAPW$Domicile_type)
mdAPW$Happy_scale_avg <- as.numeric(mdAPW$Happy_scale_avg)
mdAPW$Health_scale_avg <- as.numeric(mdAPW$Health_scale_avg)
mdAPW$Social_scale_avg <- as.numeric(mdAPW$Social_scale_avg)

### Help generating plot outputs Commented out
#ggplotgui::ggplot_shiny(mdAPW)




### Relationships among environmental variables

# Is domicile type significantly correlated to health?
## linear regression of Health_scale_avg ~ Domicile_type
lm_Health <- lm(Health_scale_avg ~ Domicile_type, data = mdAPW)
summary(lm_Health)

## Kruskal-Wallis test of Health_scale_avg ~ Domicile_type
kruskal.test(mdAPW$Health_scale_avg ~ mdAPW$Domicile_type)

## violin plot of Domicile by Health using library("ggplot2")
graph_violin_Domicile_by_Health <- ggplot(mdAPW, aes(x = Domicile_type, y = Health_scale_avg, colour = Domicile_type)) +
  geom_violin(adjust = 1) +
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  labs(x = 'Domicile Type', y = 'Health') +
  ggtitle('Domicile Type by Health') +
  theme_bw() +
  theme(
    text = element_text(family = 'Times'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph_violin_Domicile_by_Health
### Save plot
ggsave('Figures/graph_violin_Domicile_by_Health.png', graph_violin_Domicile_by_Health, width = 14, height = 14, units = 'cm')


# Is domicile type significantly correlated to happiness?
# linear regression of Happy_scale_avg ~ Domicile_type
lm_Happiness <- lm(Happy_scale_avg ~ Domicile_type, data = mdAPW)
summary(lm_Happiness)

## Kruskal-Wallis test of Happy_scale_avg ~ Domicile_type
kruskal.test(mdAPW$Happy_scale_avg ~ mdAPW$Domicile_type)

## violin plot of Domicile by Happy_scale_avg using library("ggplot2")
graph_violin_Domicile_by_Happiness <- ggplot(mdAPW, aes(x = Domicile_type, y = Happy_scale_avg, colour = Domicile_type)) +
  geom_violin(adjust = 1) +
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  labs(x = 'Domicile Type', y = 'Happiness') +
  ggtitle('Domicile Type by Happiness') +
  theme_bw() +
  theme(
    text = element_text(family = 'Times'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph_violin_Domicile_by_Happiness
### Save plot
ggsave('Figures/graph_violin_Domicile_by_Happiness.png', graph_violin_Domicile_by_Happiness, width = 14, height = 14, units = 'cm')


# Is domicile type significantly correlated to social connectedness?
# linear regression of Social_scale_avg ~ Domicile_type
lm_Social <- lm(Social_scale_avg ~ Domicile_type, data = mdAPW)
summary(lm_Social)

## Kruskal-Wallis test of Social_scale_avg ~ Domicile_type
kruskal.test(mdAPW$Social_scale_avg ~ mdAPW$Domicile_type)

## violin plot of Domicile by Social_scale_avg using library("ggplot2")
graph_violin_Domicile_by_Social <- ggplot(mdAPW, aes(x = Domicile_type, y = Social_scale_avg, colour = Domicile_type)) +
  geom_violin(adjust = 1) +
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  labs(x = 'Domicile Type', y = 'Social') +
  ggtitle('Domicile Type by Social Connectedness') +
  theme_bw() +
  theme(
    text = element_text(family = 'Times'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph_violin_Domicile_by_Social
### Save plot
ggsave('Figures/graph_violin_Domicile_by_Social.png', graph_violin_Domicile_by_Social, width = 14, height = 14, units = 'cm')


# Is social connectedness significantly correlated to health?
## linear regression of Health_scale_avg ~ Social_scale_avg
lm_Health_Social <- lm(Health_scale_avg ~ Social_scale_avg, data = mdAPW)
summary(lm_Health_Social)

## Kruskal-Wallis test of Health_scale_avg ~ Social_scale_avg
kruskal.test(mdAPW$Health_scale_avg ~ mdAPW$Social_scale_avg)

## lm in ggplot Social_scale_avg by Health_scale_avg
graph_linear_Social_by_Health <- ggplot(mdAPW, aes(x = Social_scale_avg, y = -1*Health_scale_avg)) +
  geom_smooth(method = "lm", col = "red") +
  geom_jitter(width = 0.05, height = 0.05) +
  labs(x = 'Social Scale', y = 'Health Scale') +
  ggtitle('Social Connectedness by Health Scale') +
  theme_classic()
graph_linear_Social_by_Health
# Save plot
ggsave('Figures/graph_linear_Social_by_Health.png', graph_linear_Social_by_Health, width = 14, height = 14, units = 'cm')


# Is happiness significantly correlated to health?
## linear regression of Health_scale_avg ~ Happy_scale_avg
lm_Health_Happy <- lm(Health_scale_avg ~ Happy_scale_avg, data = mdAPW)
summary(lm_Health_Happy)

## Kruskal-Wallis test of Happy_scale_avg ~ Domicile_type
kruskal.test(mdAPW$Health_scale_avg ~ mdAPW$Happy_scale_avg)

## lm in ggplot Happy_scale_avg by Health_scale_avg
graph_linear_Happy_by_Health <- ggplot(mdAPW, aes(x = Happy_scale_avg, y = -1*Health_scale_avg)) +
  geom_smooth(method = "lm", col = "red") +
  geom_jitter(width = 0.05, height = 0.05) +
  labs(x = 'Social Scale', y = 'Health Scale') +
  ggtitle('Happiness by Health Scale') +
  theme_classic()
graph_linear_Happy_by_Health
# Save plot
ggsave('Figures/graph_linear_Happy_by_Health.png', graph_linear_Happy_by_Health, width = 14, height = 14, units = 'cm')


####Diversity
#### Remove sites with less than 100 observations for diversity analysis
datAPW <- datAPW[rowSums(datAPW) > 99,]

### Subset metadata to match
mdAPW <- subset(mdAPW, row.names(mdAPW) %in% row.names(datAPW))

### Normalize the data for diversity analysis
datAPW2 <- (datAPW/rowSums(datAPW))*1000
summary(rowSums(datAPW2))
summary(colSums(datAPW2))


#### Species-individual curve
dat.specaccum <- specaccum (datAPW2, method = "random")
plot(dat.specaccum)
# Save plot
ggsave('Figures/Species-individual_curve.png', plot(dat.specaccum), width = 14, height = 14, units = 'cm')


#### Measures of alpha diversity
dat.specnumber <- specnumber(datAPW2) ## Number of species
dat.rowsums <- rowSums(datAPW2!=0) ## Number of non-zero elements in each row. Note that this is the same as above.
dat.shannon <- diversity(datAPW2) ## Shannon diversity
dat.ens <- exp(dat.shannon) ## Effective number of species
dat.ensr <- round(dat.ens) # Round to the nearest integer 
mdAPW <- cbind(mdAPW, effective_species = dat.ensr) # Add to metadata

### Combine into one table for easy graphing 
dat.alpha <- cbind.data.frame(Quadrat=c(1:146),dat.specnumber,dat.shannon,dat.ens, dat.ensr)

### Make graphs in ggplot2
plot1 <- ggplot(data=dat.alpha, aes(x=Quadrat, y=dat.specnumber)) +
  geom_point()
plot2 <- ggplot(data=dat.alpha, aes(x=Quadrat, y=dat.shannon)) +
  geom_point()
plot3 <- ggplot(data=dat.alpha, aes(x=Quadrat, y=dat.ens)) +
  geom_point()
plot4 <- ggplot(data=dat.alpha, aes(x=Quadrat, y=dat.ensr)) +
  geom_point()

### plot the 4 graphs next to each other
grid.arrange(plot1, plot2, plot3, plot4, ncol=4)

# Save plot
ggsave('Figures/Diversity_x4_plots.png', grid.arrange(plot1, plot2, plot3, plot4, ncol=4), width = 14, height = 14, units = 'cm')


## generalized linear model with your effective number of species to see alpha diversity 
glm1 <- glm(effective_species ~ Domicile_type + Happy_scale_avg + Health_scale_avg + Social_scale_avg, family = poisson, data=mdAPW)
summary(glm1)

plot(allEffects(glm1))

ggsave('Figures/Effective_number_of_species_by_metadata.png', plot(allEffects(glm1)), width = 14, height = 14, units = 'cm')





#### Measures of beta diversity 

### NMDS First using Bray-Curtis
## set a seed to make the results reproducible
set.seed(100)

## Bray-Curtis is the default metric, k = 2 dimensions
datAPW2.bc.nmds <- metaMDS(datAPW2, k=2, trymax=50) 

## Bray-Curtis is the default metric, k = 3 dimensions
datAPW2.bc.nmds <- metaMDS(datAPW2, k=3, trymax=50) 

## Plot that shows names, Bray-Curtis
BrayCurtis <- ordiplot(datAPW2.bc.nmds, xlab="Bray-Curtis NMDS Axis 1", ylab="Bray-Curtis NMDS Axis 2", type = "t",display = "sites", cex = 0.25, main="Bray-Curtis")
## Save plot, Bray-Curtis
ggsave('Figures/Bray-Curtis.png', ordiplot(datAPW2.bc.nmds, xlab="Bray-Curtis NMDS Axis 1", ylab="Bray-Curtis NMDS Axis 2", type = "t",display = "sites", cex = 0.25, main="Bray-Curtis"), width = 14, height = 14, units = 'cm')

## shows how your dissimilarity fits with ordination distance
stressplot(datAPW2.bc.nmds) 

## Save plot, Stressplot
ggsave('Figures/BC_Stressplot.png', graph, width = 14, height = 14, units = 'cm')

## Now using the binary version of Jaccard
datAPW2.jb.nmds <- metaMDS(datAPW2, k=2, trymax=50, distance= "jaccard", binary=T) 

## Plot that shows names, Jaccard NMDS
Jaccard <- ordiplot(datAPW2.jb.nmds, xlab="Jaccard NMDS Axis 1", ylab="Jaccard NMDS Axis 2", type = "t",display = "sites", cex = 0.25, main="Jaccard")
## Save plot, Jaccard
ggsave('Figures/Jaccard.png', ordiplot(datAPW2.jb.nmds, xlab="Jaccard NMDS Axis 1", ylab="Jaccard NMDS Axis 2", type = "t",display = "sites", cex = 0.25, main="Jaccard"), width = 14, height = 14, units = 'cm')







## Shows how your dissimilarity fits with ordination distance
stressplot(datAPW2.jb.nmds)

## Save plot, Stressplot Jaccard NMDS
ggsave('Figures/Stressplot_Jaccard_NMDS.png', graph, width = 14, height = 14, units = 'cm')

# Is beta diversity significantly correlated to country or domicile type?
### PERMANOVA to see if your beta diversity correlates with Country or Domicile Type
datAPW2.ad <- adonis(datAPW2 ~ Domicile_type + Country, data=mdAPW)
datAPW2.ad

### Mantel test to look at correlations with your community composition (in terms of age and sex) with your continuous variables
### Mantel test for continuous variables (this is a linear test)
# Is there a correlation between "species" and happiness across sites?
happy.dist <- vegdist(mdAPW$Happy_scale_avg, method="euclidean") # need to make this into a distance matrix (Euclidean)
datAPW2.bcdist <- vegdist(datAPW2) # need to make this into a distance matrix too (Bray-Curtis)
happy.bc.man <- mantel(datAPW2.bcdist,happy.dist, permutations=999) # Mantel test
happy.bc.man

## Repeat Mantel test other continuous variables, Health_scale_avg
# Is there a correlation between "species" and health across sites?
health.dist <- vegdist(mdAPW$Health_scale_avg, method="euclidean") # need to make this into a distance matrix (Euclidean)
datAPW2.bcdist <- vegdist(datAPW2) # need to make this into a distance matrix too (Bray-Curtis)
health.bc.man <- mantel(datAPW2.bcdist,health.dist, permutations=999) # Mantel test
health.bc.man

## Repeat Mantel test other continuous variables, Social_scale_avg
# Is there a correlation between "species" and social connectedness across sites?
social.dist <- vegdist(mdAPW$Social_scale_avg, method="euclidean") # need to make this into a distance matrix (Euclidean)
datAPW2.bcdist <- vegdist(datAPW2) # need to make this into a distance matrix too (Bray-Curtis)
social.bc.man <- mantel(datAPW2.bcdist,social.dist, permutations=999) # Mantel test
social.bc.man





# Individual level SPSS Data to R comparison
# Load data library("foreign")
spss_data <- read.spss(file = "C:/Users/annwozniak/Desktop/EEB_Final_Project_Wozniak/Raw_Data/ESS_Full_EEB.sav")
# view the first few rows of the data
head(spss_data)
# view the basic descriptive for the entire data set by using summary
summary(spss_data)


# Logistic regression for all variables to test for signigicance
# Run a logistic regression, controlling for demographics, Domicile Type by all factors
SPSS_GLM_All <- glm(Calc_Health ~ domicil + happy + sclmeet + Calc_Safe + brncntr + crmvct + ctzcntr + dvrcdev + empl + emprf14 + emprm14 + facntr + gndr + Calc_Household_Size + mocntr + partner + inmdisc + vote, data = spss_data, family = "binomial")
# Display
summary(SPSS_GLM_All)
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_ALL_LR.txt')
#output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_All)
# Stop writing to the file
sink()


# Is domicile type significantly correlated to health?
# Run a logistic regression, Calc_Health ~ domicil
SPSS_GLM_Dm_by_Calc_Health <- glm(Calc_Health ~ domicil, data = spss_data, family = "binomial")
# Display
summary(SPSS_GLM_Dm_by_Calc_Health)
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Dm_by_Calc_Health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Dm_by_Calc_Health)
# Stop writing to the file
sink()


# Run a logistic regression, Domicile by Health, X_health ~ domicil
SPSS_GLM_Dm_by_X_health <- glm(X_health ~ domicil, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Dm_by_X_health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Dm_by_X_health)
# Stop writing to the file
sink()

# Output all significant x_health
# Smaller the number, the better the health
# Rank from unhealthy to healthy
# Country village = 1.20029 + 0.15912 = 1.35941
# Town or small city = 1.20029 + 0.10727 = 1.30756
# A big city = 1.20029 
# Suburbs or outskirts of big city = 1.20029 - 0.18594 = 1.01435
# Farm or home in countryside = 1.20029 - 0.20751 = 0.99278


# Is happiness significantly correlated to health?
# Run a logistic regression, Calc_Health ~ happy
SPSS_GLM_Happy_by_Calc_Health <- glm(Calc_Health ~ happy, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Happy_by_Calc_Health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Happy_by_Calc_Health)
# Stop writing to the file
sink()

# Run a logistic regression, X_health ~ happy
SPSS_GLM_Happy_by_X_health <- glm(X_health ~ happy, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Happy_by_X_health.txt')
### output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Happy_by_X_health)
# Stop writing to the file
sink()


# Is social connectedness significantly correlated to health?
# Run a logistic regression, Calc_Health ~ sclmeet
SPSS_GLM_Social_by_Calc_Health <- glm(Calc_Health ~ sclmeet, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Social_by_Calc_Health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Social_by_Calc_Health)
# Stop writing to the file
sink()


# Run a logistic regression, X_health ~ sclmeet
SPSS_GLM_Social_by_X_Health <- glm(X_health ~ sclmeet, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Social_by_X_Health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Social_by_X_Health)
# Stop writing to the file
sink()


# Interaction, Domicile + Happy by Health
# Run a logistic regression, Calc_Health ~ domicil + happy
SPSS_GLM_Dm_Happy_by_Calc_Health <- glm(Calc_Health ~ domicil + happy, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Dm_Happy_by_Calc_Health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Dm_Happy_by_Calc_Health)
# Stop writing to the file
sink()

# Run a logistic regression, X_health ~ domicil + happy
SPSS_GLM_Dm_Happy_by_X_Health <- glm(X_health ~ domicil + happy, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Dm_Happy_by_X_Health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Dm_Happy_by_X_Health)
# Stop writing to the file
sink()


# Interaction, Domicile + Social by Health
# Run a logistic regression, Calc_Health ~ domicil + sclmeet
SPSS_GLM_Dm_sclmeet_by_Calc_Health <- glm(Calc_Health ~ domicil + sclmeet, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Dm_sclmeet_by_Calc_Health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Dm_sclmeet_by_Calc_Health)
# Stop writing to the file
sink()

# Run a logistic regression, X_health ~ domicil + sclmeet
SPSS_GLM_Dm_sclmeet_by_X_Health <- glm(X_health ~ domicil + sclmeet, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-SPSS_GLM_Dm_sclmeet_by_X_Health.txt')
# output from our regression. In order to get the results we use the summary command
summary(SPSS_GLM_Dm_sclmeet_by_X_Health)
# Stop writing to the file
sink()


### Can domicile type predict social Interaction?
# Run a logistic regression, Domicile + Social
SPSS_DM_by_Social <- glm(sclmeet ~ domicil, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-output_DM_by_Social.txt')
### output from our regression. In order to get the results we use the summary command
summary(SPSS_DM_by_Social)
# Stop writing to the file
sink()


### Can domicile type predict happiness?
# Run a logistic regression, Domicile + Happy
SPSS_DM_by_happy <- glm(happy ~ domicil, data = spss_data, family = "binomial")
# Start writing to an output file
sink('Output/analysis-output_DM_by_Happy.txt')
### output from our regression. In order to get the results we use the summary command
summary(SPSS_DM_by_happy)
# Stop writing to the file
sink()