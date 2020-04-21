# Example plots for README.md
# Jenna Hershberger
# jmh579@cornell.edu
# March 26, 2020

library(tidyverse)
library(ggpubr)
library(LaCroixColoR)
library(wesanderson)

load("./data/ikeogu.2017.rda")

#### Example data distribution plot for README.md ####

# Standalone plot
# ikeogu.2017 %>% dplyr::select(-starts_with("X")) %>% group_by(study.name) %>%
#   gather(trait, value, c(DMC.oven:TCC), na.rm = T) %>%
#   ggplot(aes(x= study.name, y = value, fill = study.name)) +
#   scale_fill_manual(values = lacroix_palette("PeachPear", n = 7, type = "continuous"))+
#   #scale_fill_manual(values=wes_palette(name="Zissou1", n = 7, type = "continuous"), name = "Study") +
#   facet_wrap(~ trait, scales='free_y', nrow=2) +
#   labs(title = "DMC reference value distributions for each example study", x = "Study Name") +
#   geom_violin() + #geom_boxplot(width=0.1) +
#   theme_bw() +
#   theme(legend.position = "none")

dmc.plot <- ikeogu.2017 %>%
  dplyr::select(-starts_with("X")) %>%
  group_by(study.name) %>%
  ggplot(aes(x= study.name, fill = study.name, y = DMC.oven)) +
  scale_fill_manual(values = lacroix_palette("PeachPear", n = 7, type = "continuous"))+
  theme_bw() +
  geom_violin() + geom_boxplot(width = 0.15)

dmc.plot.v <- dmc.plot +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Root dry matter content (%)")

dmc.plot.h <- dmc.plot +
  theme(legend.position = "none") +
  labs(y = "Root dry matter content (%)", x = "Study")

tcc.plot <- ikeogu.2017 %>%
  dplyr::select(-starts_with("X")) %>%
  group_by(study.name) %>%
  ggplot(aes(x= study.name, y = TCC, fill = study.name)) +
  scale_fill_manual(values = lacroix_palette("PeachPear", n = 7, type = "continuous"))+
  labs(y = expression(paste("Total carotenoid content (", mu, "g/g)")), x = "Study") +
  geom_violin() + geom_boxplot(width = 0.15) +
  theme_bw() +
  theme(legend.position = "none")

reference.distributions.vertical <- ggarrange(dmc.plot.v + rremove("x.text"), tcc.plot ,
                                     labels = c("A", "B"),
                                     ncol = 1, nrow = 2)
ggsave(reference.distributions.vertical,
       filename = "./man/figures/example_ref_dists_v.png", width = 4, height = 7, units = "in", bg = "transparent")

reference.distributions.horizontal <- ggarrange(dmc.plot.h, tcc.plot, labels = c("A", "B"), ncol = 2, nrow = 1)

ggsave(reference.distributions.horizontal,
       filename = "./man/figures/example_ref_dists_h.png", width = 7, height = 3, units = "in", bg = "transparent")


#### Model performance figure for README.md ####
load("./data/ikeogu.2017.rda")

test1 <- ikeogu.2017 %>% rename(reference = DMC.oven) %>% rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% na.omit() %>%
  TestModelPerformance(., num.iterations = 10,preprocessing = F,wavelengths = 350:2500)

C16Mcal <- ikeogu.2017 %>% filter(study.name == "C16Mcal") %>% rename(reference = DMC.oven) %>%
  rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% na.omit()
C16Mval <- ikeogu.2017 %>% filter(study.name == "C16Mcal") %>% rename(reference = DMC.oven) %>%
  rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% na.omit()

test2 <-  TestModelPerformance(train.data = C16Mcal, test.data = C16Mval,
                               num.iterations = 10, preprocessing = F, wavelengths = 350:2500)

test3 <-  TestModelPerformance(train.data = C16Mcal, test.data = C16Mval,
                               num.iterations = 50, preprocessing = T, wavelengths = 350:2500)

test4 <-  TestModelPerformance(train.data = C16Mcal, test.data = C16Mval,
                               num.iterations = 50, preprocessing = T, output.summary = F,
                               wavelengths = 350:2500)

write.csv(test3, "./data-raw/C16M_test.csv", row.names = F)
write.csv(test4, "./data-raw/C16M_test_nonsummary.csv", row.names = F)

unique(test4$Pretreatment)
test4.1 <- test4
test4.1$Pretreatment <- factor(test4$Pretreatment, levels = unique(test4$Pretreatment))

testplot <- test4.1 %>% group_by(Pretreatment) %>%
  summarize(`Median RMSE` = median(RMSE)) %>%
  full_join(test4.1) %>%
  ggplot(aes(y=RMSE, x = Pretreatment, fill = `Median RMSE`)) +
  geom_boxplot() + labs(title = "DMC prediction model performance") +
  scale_fill_gradientn(colors = wes_palette("Zissou1", type = "continuous")) +
#  scale_fill_gradientn(colors = lacroix_palette("PeachPear", type = "continuous"))+
  theme_minimal() + theme(axis.text.x = element_text(angle = 45,
                                                     size = 8,
                                                     hjust = 0.7))
testplot
ggsave(testplot, filename = "./man/figures/testplot1.png", width = 7, height = 4, units = "in", bg = "transparent")


# TODO needs work
manuscript_plot <- ggarrange(testplot,
                             ggarrange(dmc.plot.h, tcc.plot,
                                       labels = c("B", "C"),
                                       ncol = 2, nrow = 1),
                             nrow = 2,
                             labels = c("A"))




