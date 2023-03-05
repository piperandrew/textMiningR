######### GGPLOT CODE   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

ggplot(df, aes(x=year, y=percent, color = genre)) +
  #geom_point() +
  geom_line() +
  #scale_colour_manual(values=c("black", "grey60")) +
  scale_colour_manual(values=c("deeppink", "grey40")) +
  #theme_bw() + 
  theme_classic() + 
  xlab("Year") +
  ylab("Percent") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = c(0.8, 0.2)) +
  theme(legend.text = element_text(size=12))+
  #theme(legend.position="bottom") +
  #ggtitle("Percentage of ")+
  labs(caption="Source: ")

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



