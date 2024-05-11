######### GGPLOT CODE   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

ggplot(history.df, aes(x=period, y=mean, color=genre)) +
  geom_point(shape = 15, size = 3) +
  geom_line(linetype = "dotted") +
  #scale_colour_manual(values=c("black", "grey60")) +
  scale_colour_manual(values=c("deeppink", "grey40")) +
  #theme_bw() + 
  theme_classic() + 
  xlab("Period") +
  ylab("Model Accuracy") +
  #theme(legend.title = element_blank()) +
  #theme(legend.position = c(0.8, 0.2)) +
  #theme(legend.text = element_text(size=12))+
  #theme(legend.position="bottom") +
  #ggtitle("Percentage of ")+
  #labs(
  #  caption="Source:",
  #  title="Your Main Title Here",  # Add your main title
  #  subtitle="Your Subtitle Here"  # Add your subtitle
  #)

#bar graph
ggplot(year.df, aes(x=Date, y=Frequency)) +
  geom_bar(stat="identity") +
  theme_classic() + 
  xlab("Year") +
  ylab("Number of Books") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = c(0.8, 0.2)) +
  theme(legend.text = element_text(size=12))+
  #theme(legend.position="bottom") +
  #ggtitle("Percentage of ")+
  labs(caption="Source: ")

#bar graph with BW patterns 
library(ggpattern)

ggplot(b_filtered, aes(x=reorder(Feature, -Freq), y=Freq)) +
  geom_bar_pattern(
    aes(pattern = Category), # Define pattern by Category
    stat = "identity",
    fill = "white", # Ensure base fill is white
    colour = "grey60", # Outline color of the bars
    #pattern = "stripe", # Default pattern, you can remove this line if using scale_pattern_manual
    pattern_density = 0.05, # Adjust density of the pattern
    pattern_spacing = 0.02, # Adjust spacing between pattern lines
    pattern_key_scale_factor = 0.7 # Adjust the scale of the pattern in the legend
  ) +
  #scale_pattern_fill_manual(values=c("stripe", "dot", "plaid")) + # Manually set patterns for each category
  theme_classic() + 
  xlab("Narrative Feature") +
  ylab("Number of Times Chosen") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + # Rotate x-axis text
  labs(caption="Source: GPT-4")

#boxplot
#bar graph
ggplot(df, aes(x=Class, y=value, fill=variable)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
  #scale_fill_brewer(palette="Dark2") +
  theme_classic() + 
  xlab("Category") +
  ylab("Frequency") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = c(0.8, 0.2)) +
  #theme(legend.text = element_text(size=12))+
  #theme(legend.position="bottom") +
  #ggtitle("Percentage of ")+
  labs(caption="Source: CONLIT")

#multiple plots
#save as p1, p2 and set number of columns and rows
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

library(patchwork)
combined_plot <- p1 + p2 + p3 +
  plot_layout(nrow = 2, ncol = 2) #+
#plot_annotation(title = "Wikiplots Data", 
#                theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))


#to melt a wide format into long
library(reshape2)
#id.vars = the variable(s) you want to keep
#every other column will be melted into a single column called "value"
#the old columns will be melted into a single column called "variable"
ex<-melt(ex.work[,33:35], id.vars=c("word.count"))
ggplot(data=ex, aes(x=word.count, y=value, color=variable)) +
  geom_line() +
  theme_classic()+
  theme(legend.position="none")+
  xlab("Word Count") +
  ylab("KLD") +
  #ggtitle(levels(factor(kld.all$work))[n]) +
  labs(caption="Source: CONLIT")

