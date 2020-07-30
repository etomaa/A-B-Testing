library(tidyverse)

# Visualize the results ---------------------------------------------------

#Let's create our dataframe
ABTdata = data.frame(
  variant = factor(rep(c("A", "B"), each=200)),
  conf_interval = c(rnorm(200, 0.02773925, 0.005891128), rnorm(200, 0.05068493, 0.007793573))
)
head(ABTdata, 4)

#Let's create another sub-dataframe
pe = data.frame(variant = c("A","B"), conversion_rate = round(c(conv_rate_A, conv_rate_B),4), lower_confidence = round(c(ci_lower_A,ci_upper_A),4))
pe

#Let's plot the distributions
a <- ggplot(ABTdata, aes(x = conf_interval))

#Change fill color by variant
a + geom_density(aes(fill = variant), alpha = 0.3) +
  geom_vline(aes(xintercept = lower_confidence[1], color = variant),
             data = pe, linetype = "dashed") +
  geom_vline(aes(xintercept = lower_confidence[2], color = variant),
             data = pe, linetype = "dashed") +
  geom_vline(aes(xintercept = conversion_rate, color = variant),
             data = pe, linetype = "dashed") +
  scale_color_manual(values = c("#b8b7b8", "#b8b7b8"))+
  scale_fill_manual(values = c("#e9e8e9", "#4b779d")) +
  annotate(geom="text", x= 0.0507,  y=29, 
           hjust = 0,     # zero left justifies the text annotation horizontally. 0.5 is center, 1 is right justified.  
           vjust = 0.5,   # > +0.1 range pushes the text annotation down vertically
           #alpha = 0.2,  #alpha reduces the transparency of the text
           label="CR B: 5.06%",
           fontface = "bold",
           colour = "black",
           angle = 90,
           family="Century Gothic",
           size = 4.0) +
  annotate(geom="text", x= 0.0277,  y=35, 
           hjust = 0,     # zero left justifies the text annotation horizontally. 0.5 is center, 1 is right justified.  
           vjust = 0.5,   # > +0.1 range pushes the text annotation down vertically
           #alpha = 0.2,  #alpha reduces the transparency of the text
           label="CR A: 2.77%",
           fontface = "bold",
           angle = 90,
           colour = "black",
           family="Century Gothic",
           size = 4.0)+
  annotate(geom="text", x= 0.0403,  y=24, 
           hjust = 0,     # zero left justifies the text annotation horizontally. 0.5 is center, 1 is right justified.  
           vjust = 0.5,   # > +0.1 range pushes the text annotation down vertically
           #alpha = 0.2,  #alpha reduces the transparency of the text
           label="95%",
           #fontface = "bold",
           angle = 90,
           colour = "black",
           family="Century Gothic",
           size = 4.0)+
  annotate(geom="text", x= 0.0164,  y=24, 
           hjust = 0,     # zero left justifies the text annotation horizontally. 0.5 is center, 1 is right justified.  
           vjust = 0.5,   # > +0.1 range pushes the text annotation down vertically
           #alpha = 0.2,  #alpha reduces the transparency of the text
           label="95%",
           #fontface = "bold",
           angle = 90,
           colour = "black",
           family="Century Gothic",
           size = 4.0)+
  labs(title = "Expected Distributions of Variants A and B",
       subtitle = "Variant B's observed conversion rate (5.07%) was 82.72% higher than Variant B's conversion rate (2.77%).\nYou can be 95% confident that this result is a consequence of the changes made and not a result of random chance.",
       x = "Confidence Interval",
       y = "",
       caption = "Etoma Egot") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.ticks.y = element_blank(), #remove y axis ticks
        axis.title.y = element_blank()) + #remove y axis title
  theme(plot.title = ggplot2::element_text(family="Century Gothic",
                                           size=20,
                                           #face="bold",
                                           color="#002849")) +
  theme(plot.subtitle = ggplot2::element_text(family="Century Gothic",
                                              size=16,
                                              face="bold",
                                              color = "#4b779d"))+
  theme(panel.grid.major.y = ggplot2::element_line(color="#FAFAFA"))



#Visualizing the pool using a normal distribution

library(extrafont)
ABTdata_pool = data.frame(Conf_interval = rnorm(n = 10000, mean = 0, sd = SE_pool))

#Let's plot the distributions
b <- ggplot(ABTdata_pool, aes(x = Conf_interval))

b + geom_histogram(aes(y = ..density..),
                   colour="black", fill="white", binwidth = 0.00029) +
  geom_density(alpha = 0.2, fill = "#FF6666")+
  theme_WD()
