### ----------------------------------------------------------------------------------------------
### Chart showing a QB's completion percentage by depth when hit
### relative to league average
### ----------------------------------------------------------------------------------------------

library(tidyverse)

pass_plays <- read_csv("data/__pass_plays.csv")

### Elias charts dumps throw aways into the 0 air yards bucket.-----------------------------------
### This biases the data when binning, so we remove it.

pass_plays_no_spikes_qb_hit <- pass_plays %>%
   mutate(spike = ifelse(str_detect(description, "spiked"), 1, 0)) %>%
   filter(spike == 0,
          qb_hits >= 1)

pass_plays_no_spikes_no_hits <- pass_plays %>%
   mutate(spike = ifelse(str_detect(description, "spiked"), 1, 0)) %>%
   filter(spike == 0,
          qb_hits == 0)

ggplot(pass_plays_no_spikes_qb_hit, aes(x = air_yards, y = passing_cmp)) +
   theme_minimal() +
   theme(plot.title=element_text(size=16, hjust=0, vjust=-1),
         plot.subtitle=element_text(size=12, hjust=0)) +
   geom_smooth(
      fill = '#CCCCCC',
      alpha = .2,
      color = '#018417',
      size = .6,
      span = .5,
      se = FALSE
   ) +
   geom_smooth(
      data = pass_plays_no_spikes_no_hits,
      aes(y = passing_cmp, x = air_yards),
      fill = '#CCCCCC',
      alpha = .2,
      color = '#FFB704',
      size = .6,
      span = .5,
      se = FALSE
   ) +
   scale_x_continuous(
      limit = c(-5, 50),
      breaks = c(seq(
         from = -5,
         to = 50,
         by = 5
      )),
      minor_breaks = NULL
   ) +
   scale_y_continuous(
      limit = c(0, 1),
      breaks = c(seq(
         from = 0,
         to = 1,
         by = .1
      )),
      minor_breaks = NULL,
      labels = scales::percent
   ) +
   labs(x = "Depth of Target", y = "Completion Percentage",
        title = "Hits on the QB negatively impact passing at all depths tktktk",
        subtitle = '2009-2016 league average completion percentage in orange',
        caption = "Source: NFL, Elias Sports Bureau")

### magic incantation to save the plot to disk -----------------------------------------------------

dev.copy(png,'total_defensive_dvoa.png')
dev.off()

### Verify that these are hits with no sack --------------------------------------------------------

colnames(pass_plays)

hits_and_sacks <- pass_plays %>%
   filter(qb_hits == 1) %>%
   select(qb_hits, sacks)
