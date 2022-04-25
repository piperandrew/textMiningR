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


