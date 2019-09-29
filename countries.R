#Loading needed libraries
library(readr)
library(dplyr)
#Reading needed data
countries_df <- read_csv("country_profile_variables.csv")
countries_df <- as.data.frame(countries_df)
#Let's see whats inside
glimpse(countries_df)

#Let's use country names as rownames
rownames(countries_df) <- countries_df$country

#Let's use only numerics
num_cols <- c()
for (col in 1:ncol(countries_df)){
  num_cols <- c(num_cols, is.numeric(countries_df[,col]))
}
num_cols
sum(num_cols)
countries_ltd <- countries_df[,num_cols]
glimpse(countries_ltd)
summary(countries_ltd)

#Remove the last column
countries_ltd <-countries_ltd[,1:15]
rownames(countries_ltd) -> countries_ltd$country
#Let's use only the countries with no missings
countries_ltd <- countries_ltd %>%
  filter(`Sex ratio (m per 100 f, 2017)`>=0 &
           `Individuals using the Internet (per 100 inhabitants)`>=0 &
         `GDP: Gross domestic product (million current US$)` >=0&
         `Economy: Industry (% of GVA)`>=0 &
         `Economy: Services and other activity (% of GVA)`>=0 &
         `Agricultural production index (2004-2006=100)`>=0 &
           `Food production index (2004-2006=100)` >=0 &
           `Health: Total expenditure (% of GDP)` >=0 &
           `Seats held by women in national parliaments %`>=0 &
           `CO2 emission estimates (million tons/tons per capita)`>=0 &
           `Energy production, primary (Petajoules)`>=0 
           )
rownames(countries_ltd) <- countries_ltd$country
#Only numerics
countries_ltd <- countries_ltd[,1:15]
#Normalization
range0to1 <- function(x){(x-min(x))/(max(x)-min(x))}
countries_ltd_scaled <- countries_ltd

for (col in 1:ncol(countries_ltd_scaled)){
  countries_ltd_scaled[,col] <- range0to1(countries_ltd_scaled[,col])
}
  
#PCA plot
pc <- princomp(as.matrix(countries_ltd_scaled))
biplot(pc,cex=0.5, main='PCA')
plot(pc)
summary(pc)
pc$loadings

# Comp.1 = niezurbanizowanie, małe GDP per capita, niski udział usług w GVA -> ogółem "niski poziom rozwoju"
# Comp.2 = niski udział przemysłu w GVA, niski poziom urbanizacji -> podobnie jak powyżej ale tu nacisk na brak przemysłu
# Comp.3 = przede wszystkim stosunkowo duża liczba kobiet w parlamencie oraz wysoka "rolniczość" - wysoki wskaźnik
# produkcji rolnej oraz produkcji żywności
# Comp.4 = przede wszystkim stosunkowo duża liczba kobiet w parlamencie oraz "nierolniczość"
# Comp.5 = przede wszystkim mała dostępność do internetu i niska emisja CO2
# Comp.6 = małe GDP per capita, wysoka urbanizacja, niska produkcja energii
head(pc$scores)
#scree plot (wykres osypiska)
plot(pc,type="lines")
pca_scores <- as.data.frame(pc$scores[,1:6])
###########
library(plotly)
#k-means
set.seed(20)
COMPONENTS_FOR_KMEANS=6
pca_Cluster <- kmeans(pc$scores[,1:COMPONENTS_FOR_KMEANS], 10, nstart = 20)

Cluster_df <- data.frame(pc$scores[,1:COMPONENTS_FOR_KMEANS], pca_Cluster$cluster, rownames(countries_ltd_scaled))

#Characteristics of particular clusters
countries_ltd_scaled %>% mutate(country = rownames(countries_ltd)) %>%
  left_join(Cluster_df, by = c("country"="rownames.countries_ltd_scaled.")) ->joined_df

cbind(joined_df[1:15],joined_df$pca_Cluster.cluster) -> joined_df_red
joined_df_red %>%
  group_by(`joined_df$pca_Cluster.cluster`) %>%
  summarise_all(funs(mean)) -> clust_stats

clust_stats
#Wnioski:
# 1 = najmniejsza gęstość zaludnienia
# 2 = kraje z największą populacją, wysokim GDP, najwyższą emisją CO2
# 3 = największy udział przemysłu w GVA
# 4 = najmniejsze GDP, najmniejszy udział przemysłu w GVA
# 5 = najmniejsza urbanizacja
# 6 = najmniejsze GDP per capita
# 7 = najmniejsza liczba mężczyzn na 100 kobiet
# 8 = największa urbanizacja, największa liczba mężczyzn na 100 kobiet, wysoki udział przemysłu w GVA, 
# najwyższy food production index, najmniejsza liczba kobiet w parlamencie
# 9 = największa gęstość zaludnienia, największe GDP per capita
# 10 = największa liczba kobiet w parlamencie

# UWAGA: w k-means bierzemy pod uwagę 6 głównych składowych,
# natomiast w wykresie 3D bierzemy 3 główne składowe
# stąd różnice w odległościach np. Chin od USA

#2D plot
library(ggbiplot)
library(ggplot2)
ggbiplot(pc,cex=0.5, groups = as.factor(Cluster_df$pca_Cluster.cluster))  

#3D plot with clusters in colors
pca_scores$country <- rownames(pca_scores)
pca_scores$cluster <- Cluster_df$pca_Cluster.cluster
p <- plot_ly(pca_scores, x = ~Comp.1, y = ~Comp.2, z = ~Comp.3, text = ~paste('Country:', country, 'Cluster:', cluster), 
             color = ~cluster, colors = c('#BF382A', '#0C4B8E', '#61f464', '#e63be7', '#ff581f')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'C1'),
                      yaxis = list(title = 'C2'),
                      zaxis = list(title = 'C3')))
p


