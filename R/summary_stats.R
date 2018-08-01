avgminmax_bin = feather::read_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/avgminmax_bin.feather")

## produce histogram of daily average temp
lowerbound = min(acc$AVGMINMAX)
upperbound = max(acc$AVGMINMAX)
breaks = c(lowerbound, seq(10, 90, by=10), upperbound)
break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")

acc %>%
  dplyr::mutate(`value_label`=dplyr::case_when(`AVGMINMAX` < 10 ~ "<10",
                                               `AVGMINMAX` < 20 ~ break_labels[2],
                                               `AVGMINMAX` < 30 ~ break_labels[3],
                                               `AVGMINMAX` < 40 ~ break_labels[4],
                                               `AVGMINMAX` < 50 ~ break_labels[5],
                                               `AVGMINMAX` < 60 ~ break_labels[6],
                                               `AVGMINMAX` < 70 ~ break_labels[7],
                                               `AVGMINMAX` < 80 ~ break_labels[8],
                                               `AVGMINMAX` < 90 ~ break_labels[9],
                                               TRUE ~ break_labels[10]
                                               )) %>%
  dplyr::mutate(`value_label`=factor(`value_label`, levels=break_labels)) %>%
  ggplot2::ggplot(ggplot2::aes(x=`value_label`)) +
  ggplot2::geom_bar() +
  ggtitle("Distribution of daily mean (average of min and max) temperature") +
  xlab("daily mean temperature (average of min and max)") +
  ggplot2::theme_bw()
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/avgminmax_bin_barstyle.png", width=6, height=4, units="in")

## produce histogram of daily average temp
acc %>%
  ggplot2::ggplot(ggplot2::aes(x=`AVGMINMAX`)) +
  ggplot2::geom_histogram(binwidth=10)

acc %>%
  summary()
