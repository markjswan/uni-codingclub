LPI <- read.csv("data-vis/LPIdata_CC.csv")

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)

LPI2$year <- parse_number(LPI2$year)

# When manipulating data it's always good to check if the variables have stayed how we want them
# Use the str() function
str(LPI2)



unique(LPI2$'Common Name')

vulture <- filter(LPI2, Common.Name == "Griffon vulture / Eurasian griffon")
head(vulture)

# There are a lot of NAs in this dataframe, so we will get rid of the empty rows using na,omit()
vulture <- na.omit(vulture)
vulture$abundance <- as.numeric(vulture$abundance)
str(vulture)
# Now we want to compare R graphics and ggplot2
# 1. With base R graphics
base_hist <- hist(vulture$abundance)

# 2. With ggplot2
vulture_hist <- ggplot(vulture, aes(x = abundance)) + geom_histogram()

# Calling the object to display it in the plot viewer
vulture_hist

# With brackets: you create and display the graph at the same time
(vulture_hist <- ggplot(vulture, aes(x = abundance)) + geom_histogram())


###############
# Beautify Data
###############

(vulture_hist <- ggplot(vulture, aes(x=abundance)) + geom_histogram(binwidth = 250, colour = "#8B5A00", fill = "#CD8500") + 
   geom_vline(aes(xintercept = mean(abundance)), colour = "red", linetype = "dashed", size=1) + theme_bw() +
  ylab("Count\n") + 
   xlab("\nGriffon vulture abundance") + 
   theme(axis.text = element_text(size = 12),
         axis.title.x = element_text(size = 14, face = "plain"),
         panel.grid = element_blank(), 
         plot.margin = unit(c(1,1,1,1), units = , "cm")))

##################################
# Learning how to use colourpicker
##################################

install.packages("colourpicker")
c("#FFFFFF", "#FFFFFF", "#FFFFFF")


#################################
# Scatter plot
#################################

# Create a scatter plot to examine how Griffon vulture
# populations have changed between 1970 and 2017 in Croatia and Italy

# Filtering the data to get records only from Croatia and Italy using the filter()
vultureITCR <- filter(vulture, Country.list %in% c("Croatia", "Italy"))

# Using default ggplot2 graphics 
(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list))
 + geom_point())

# Now we want to make the graph more readable
(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list))
  + geom_point(size = 2)
  + geom_smooth(method = "lm", aes(fill = Country.list))
  + theme_bw()
  + scale_fill_manual(values = c("#EE7600", "#00868B"))
  + scale_colour_manual(values = c("#EE7600", "#00868B"), 
                        labels = c("Croatia", "Italy")) 
  + ylab("Griffon vulture abundance\n")
  + xlab("\nYear")
  + theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = c(0.9, 0.9)))

# Boxplot ----
(vulture_boxplot <- ggplot(vultureITCR, aes('Country list', abundance)) + geom_boxplot())

# Beautifying
(vulture_boxplot <- ggplot(vultureITCR, aes(Country.list, abundance)) + geom_boxplot(aes(fill = Country.list)) + 
    theme_bw() +
    scale_fill_manual(values = c ("#EE7600", "#00868B")) +
    scale_colour_manual(values = c("#EE7600", "#00868B")) +
    ylab("Griffon vulture abundance\n") +
    xlab("\nCountry") +
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.position = "none"))


# Barplot ----
# Calculating species richness using pipes %>% from dplyr package
richness <- LPI2 %>% filter(Country.list %in% c("United Kingdom", "Germany", "France", "Netherlands", "Italy")) %>%
  group_by(Country.list) %>%
  mutate(richness = (length(unique(Common.Name))))

(richness_barplot <- ggplot(richness, aes(x = Country.list, y = richness)) + 
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    theme_bw() + 
    ylab("Species richness\n") +
    xlab("\nCountry") + 
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust =  1), 
          axis.text.y = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm")))


# Grid Arrange ----
grid.arrange(vulture_hist, vulture_scatter, vulture_boxplot, ncol = 1)

(panel <- grid.arrange(vulture_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +
                                                theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")), 
                                              vulture_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
                                                theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")), 
                                              vulture_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
                                                theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
                                                theme(legend.text = element_text(size = 12, face = "italic"),               
                                                      legend.title = element_blank(),                                   
                                                      legend.position = c(0.85, 0.85)),
                                              ncol = 1))


# Saving Graph Images ----
ggsave(panel, file = "data-vis/vulture_panel.png", width = 5, height = 12)









