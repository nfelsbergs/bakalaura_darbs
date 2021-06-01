
with_col_50 <- data.frame(graph = "with_collider"
                          , method = factor(c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate")
                                            , levels = c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate"))
                          , estimate = c(3.31/5, 4.98/5, 4.94/5, 6.10/5, 5.05/5, 4.96/5, 5.96/5, 5.93/5, 6.07/5)
                          , lower = c(-1.39/5, -0.92/5, -2.52/5, 1.51/5, 0.28/5, 0.34/5, 4.50/5, 4.48/5, 3.76/5)
                          , upper = c(8.01/5, 10.88/5, 12.39/5, 10.69/5, 9.83/5, 9.78/5, 7.42/5, 7.39/5, 8.38/5)
)

without_col_50 <- data.frame(graph = "without_collider"
                             , method = factor(c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate")
                                               , levels = c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate"))
                             , estimate = c(10.21/10, 10.01/10, 9.25/10, 9.97/10, 9.99/10, 10/10, 10.09/10, 10.16/10, 10.26/10)
                             , lower = c(10.08/10, 8.78/10, 8.24/10, 8.90/10, 9.10/10, 9.83/10, 9.86/10, 9.90/10, 10.01/10)
                             , upper = c(10.35/10, 11.25/10, 10.28/10, 11.05/10, 10.88/10, 10.18/10, 10.31/10, 10.42/10, 10.52/10)
)

full_50 <- data.frame(graph = "without_collider"
                      , method = factor(c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate")
                                        , levels = c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate"))
                      , estimate = c(7.68/10, 10/10, 9.25/10, 10/10, 9.99/10, 9.99/10, 10/10, 10/10, 7.71/10)
                      , lower = c(7.68/10, 9.75/10, 7.76/10, 9.62/10, 9.69/10, 9.69/10, 9.75/10, 9.75/10, 7.71/10)
                      , upper = c(7.68/10, 10.24/10, 10.73/10, 10.37/10, 10.30/10, 10.30/10, 10.24/10, 10.24/10, 7.71/10)
)



with_col_500 <- data.frame(graph = "with_collider"
                           , method = factor(c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate")
                                             , levels = c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate"))
                           , estimate = c(3.32/5, 4.97/5, 5.00/5, 6.13/5, 4.96/5, 4.96/5, 6.13/5, 6.13/5, 6.11/5)
                           , lower = c(1.90/5, 3.22/5, 2.77/5, 4.81/5, 3.58/5, 3.58/5, 5.68/5, 5.68/5, 4.69/5)
                           , upper = c(4.75/5, 6.72/5, 7.24/5, 7.45/5, 6.35/5, 6.35/5, 6.57/5, 6.57/5, 7.53/5)
)

without_col_500 <- data.frame(graph = "without_collider"
                              , method = factor(c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate")
                                                , levels = c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate"))
                              , estimate = c(10.22/10, 10.01/10, 9.26/10, 10.00/10, 10/10, 10/10, 10.02/10, 10.04/10, 10.40/10)
                              , lower = c(10.17/10, 9.64/10, 8.95/10, 9.68/10, 9.75/10, 9.95/10, 9.93/10, 9.84/10, 10.31/10)
                              , upper = c(10.26/10, 10.38/10, 9.56/10, 10.31/10, 10.25/10, 10.05/10, 10.12/10, 10.24/10, 10.48/10)
)

full_500 <- data.frame(graph = "without_collider"
                       , method = factor(c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate")
                                         , levels = c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate"))
                       , estimate = c(7.68/10, 10/10, 9.23/10, 10/10, 10/10, 10/10, 10/10, 10/10, 7.71/10)
                       , lower = c(7.68/10, 9.93/10, 8.78/10, 9.91/10, 9.91/10, 9.91/10, 9.93/10, 9.93/10, 7.71/10)
                       , upper = c(7.68/10, 10.07/10, 9.67/10, 10.09/10, 10.09/10, 10.09/10, 10.07/10, 10.07/10, 7.71/10)
)

plot1 <- with_col_50 %>% ggplot(aes(x=method,y = estimate)) + geom_point(lwd = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, lwd = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=14)) +
  theme(axis.text.y = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=10)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("") +
  xlab("") +
  ggtitle("N = 50")  +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 1, linetype = "dashed") + coord_flip() +
  # scale_y_continuous(breaks = c(0,1,2)) +
  ylim(-0.6,2.5)
plot1
plot2 <- with_col_500 %>% ggplot(aes(x=method,y = estimate)) + geom_point(lwd = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, lwd = 1) +
  theme_bw()  + 
  theme(axis.text.x = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=14))+ 
  theme(axis.text.y = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=10)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("") +
  xlab("") +
  ggtitle("N = 500") +
  theme(plot.title = element_text(hjust = 0.55)) +
  geom_hline(yintercept = 1, linetype = "dashed") + coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())   +
  # scale_y_continuous(breaks = c(0,1,2)) +
  ylim(-0.6,2.5)

plot2

plot3 <- without_col_50 %>% ggplot(aes(x=method,y = estimate)) + geom_point(lwd = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, lwd = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=14)) + 
  theme(axis.text.y = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=10)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("") +
  xlab("") +
  ggtitle("") +
  geom_hline(yintercept = 1, linetype = "dashed") + coord_flip()  +
  # scale_y_continuous(breaks = c(0,1,2)) +
  ylim(0.75,1.25)
plot3
plot4 <- without_col_500 %>% ggplot(aes(x=method,y = estimate)) + geom_point(lwd = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, lwd = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=14)) + 
  theme(axis.text.y = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=10)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("") +
  xlab("") +
  ggtitle("") +
  geom_hline(yintercept = 1, linetype = "dashed") + coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())   +
  # scale_y_continuous(breaks = c(0,1,2)) +
  ylim(0.75,1.25)

plot4

plot5 <- full_50 %>% ggplot(aes(x=method,y = estimate)) + geom_point(lwd = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, lwd = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=14)) +
  theme(axis.text.y = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=10)) +
  ylab("") +
  xlab("") +
  ggtitle("") +
  geom_hline(yintercept = 1, linetype = "dashed") + coord_flip() +
  # scale_y_continuous(breaks = c(0,1,2)) +
  ylim(0.75,1.25)
plot5
plot6 <- full_500 %>% ggplot(aes(x=method,y = estimate)) + geom_point(lwd = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, lwd = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=14)) + 
  theme(axis.text.y = element_text(face="bold",angle = 0, vjust = 0.5, hjust=1, 
                                   size=14)) +
  ylab("") +
  xlab("") +
  ggtitle("") +
  geom_hline(yintercept = 1, linetype = "dashed") + coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  +
  # scale_y_continuous(breaks = c(0,1,2)) +
  ylim(0.75,1.25)
plot6



# grid.newpage()
# grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))


grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3)
