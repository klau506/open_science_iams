library(ggplot2)
library(dplyr)
library(ggrepel)

colors.labels <- c("Yes:" = "#66c2a5",
                   "No:" = "#fc8d62",
                   "Does not apply:" = "#8da0cb",
                   "Yes" = "#66c2a5",
                   "No" = "#fc8d62",
                   "Does not\napply" = "#8da0cb",
                   "Models' repository\npage:" = "#c08552",
                   "Documentation in the\nMeth/SI section of\nthe related paper:\n" = "#cc8b86",
                   "Zenodo:" = "#916162",
                   "GitHub:" = "#d1be9c",
                   "GitHub:\n" = "#d1be9c",
                   "GitLab:   \n" = "#aa998f",
                   "Docker:\n" = "#cc8b86",
                   "Jupyter\nNote-\nbook:\n" = "#c08552",
                   "Lack of\ntime:" = "#d1be9c",
                   "Lack of\ntime:\n" = "#d1be9c",
                   "Lack\nof time:\n" = "#d1be9c",
                   "Lack\nof \ntime:\n" = "#d1be9c",
                   "Lack of knowladge:\n" = "#c08552",
                   "Lack of\nknowladge:\n" = "#c08552",
                   "Lack of\n knowladge:\n" = "#c08552",
                   "     Lack of\n     knowladge:\n     " = "#c08552",
                   "Sectoral\nmodel:" = "#aa998f",
                   "Sectoral\nmodel:\n" = "#aa998f",
                   "Sectoral model:\n" = "#aa998f",
                   "Sectoral model:\n  " = "#aa998f",
                   "Too heavy data:\n" = "#aa998f",
                   "Too heavy\ndata:" = "#aa998f",
                   "Too heavy\n    data:" = "#aa998f",
                   "Output values\nare robust:\n" = "#916162",
                   "First model\nperiod\ncompared\nto observed\ndata:" = "#d1be9c",
                   "Lack of\nawareness\nof this option:\n" = "#916162",
                   "Lack of\nawareness\nof this\noption:" = "#916162",
                   "Lack of\nawareness\nof this\n   option:\n     " = "#916162",
                   "Lack of\nawareness\n   of this\n     option:\n         " = "#916162",
                   "Lack of\nawareness\n   of this\n    option:\n     " = "#916162",
                   "Lack of\n   awareness\n    of this\n         option:\n            " = "#916162",
                   "Model specific\nplatform:" = "#c08552",
                   "Model\nspecific\nplatform:\n    " = "#c08552",
                   "Project specific\nplatform:" = "#d1be9c",
                   "Rshiny\n  widget:\n" = "#aa998f",
                   "Data storage:\n" = "#c08552",
                   "Version\nrelease:\n" = "#aa998f",
                   "Report\n& doc.\nsharing:" = "#916162",
                   "Doc.\n&report\nsharing:" = "#916162",
                   "Version control:\n" = "#d1be9c",
                   "Other:\n" = "#cc8b86"
)


#' do_piechart
#'
#' @param dataset dataset with 2 columns: responses and percentages.
#' @param vjust_value vertical adjustment of the figure text.
#' @param font_size size of the figure text.
#' @returns ggplot piechart representing the percentage responses.
do_piechart <- function(dataset, vjust_value = 0.5, font_size = 10) {
  pl <- ggplot(dataset, aes(x = "", y = percentage, fill = response)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = dplyr::if_else(substr(response, nchar(response), nchar(response)) != "\n",
                                         paste0(response, " ", percentage, "%"),
                                         paste0(response, percentage, "%"))),
              position = position_stack(vjust = vjust_value), size = font_size, color = "black") +
    scale_fill_manual(values = colors.labels) +
    theme_void() +
    theme(legend.position = "none")

  return(pl)
}

#' do_piechart2
#'
#' @param dataset dataset with 2 columns: responses and percentages.
#' @param font_size size of the figure text.
#' @param offset_mm separation from the pie chart center.
#' @param direction direction of the seperation from the pie chart center.
#' @returns ggplot piechart representing the percentage responses.
do_piechart2 <- function(dataset, font_size = 10, offset_mm = 2, direction = "x") {
  # calculate cumulative midpoint of each sector for label positioning as a percentage from the North
  dataset <- dataset %>%
    dplyr::mutate(cumulative_percentage = (cumulative_percentage + cumsum(percentage) - (percentage / 2) + 38) %% 100)

  pl <- ggplot(dataset, aes(x = "", y = percentage, fill = response)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text_repel(aes(label = dplyr::if_else(response == "", "",
                                               dplyr::if_else(substr(response, nchar(response), nchar(response)) != "\n",
                                               paste0(response, " ", percentage, "%"),
                                               paste0(response, percentage, "%"))),
                        y = cumulative_percentage),
                    size = font_size,
                    color = "black",
                    # convert mm to inches for ggplot adjustment
                    nudge_x = offset_mm / 25.4,
                    nudge_y = offset_mm / 25.4,
                    segment.size = 0,
                    direction = direction,
                    box.padding = 0,
                    point.padding = 0) +
    scale_fill_manual(values = colors.labels) +
    theme_void() +
    theme(legend.position = "none")

  return(pl)
}


#' do_barchart
#'
#' @param dataset dataset with 2 columns: responses and percentages.
#' @returns ggplot batchart representing the percentage responses.
do_barchart <- function(dataset) {
  pl <-
    ggplot(dataset, aes(x = 1, y = percentage, fill = response)) +
    geom_bar(stat = "identity", width = 0.4) +  # Bar plot
    geom_text(aes(label = paste0(percentage, "%")),
              position = position_stack(vjust = 0.5),
              color = "black", size = 10) +
    coord_flip() +
    scale_fill_manual(values = colors.labels) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    xlim(0.5, 1.5)

  return(pl)
}



##### - 1. Model documentation
# a) Bar
data_documentation_a <- data.frame(
  response = c("Yes:"),
  percentage = c(100)
)
pl_documentation_a <- do_barchart(data_documentation_a)

# b) How?
data_documentation_b <- data.frame(
  response = c("Models' repository\npage:"),
  percentage = c(100)
)
pl_documentation_b <- do_piechart(data_documentation_b)

# c) Why?
data_documentation_c <- data.frame(
  response = c("Does not apply:"),
  percentage = c(100)
)
pl_documentation_c <- do_piechart(data_documentation_c)



##### - 2. Model harmonization and interconnections
# a) Bar
data_harmonization_a <- data.frame(
  response = c("Yes:", "No:", "Does not apply:"),
  percentage = c(78,11,11)
)
pl_harmonization_a <- do_barchart(data_harmonization_a)

# b) How?
data_harmonization_b <- data.frame(
  response = c("Documentation in the\nMeth/SI section of\nthe related paper:\n", "Zenodo:\n"),
  percentage = c(87.5, 12.5)
)
pl_harmonization_b <- do_piechart(data_harmonization_b, vjust_value = 0.43)

# c) Why?
data_harmonization_c <- data.frame(
  response = c("Lack of\ntime:"),
  percentage = c(100)
)
pl_harmonization_c <- do_piechart(data_harmonization_c)



##### - 3. Output standardization
# a) Bar
data_standardization_a <- data.frame(
  response = c("Yes:", "No:", "Does not apply:"),
  percentage = c(56,33,11)
)
pl_standardization_a <- do_barchart(data_standardization_a)

# c) Why?
data_standardization_c <- data.frame(
  response = c("Lack of\ntime:", "Lack of\nknowladge:\n", "Sectoral\nmodel:"),
  percentage = c(25,25,50)
)
pl_standardization_c <- do_piechart(data_standardization_c, font_size = 9)




##### - 4. Output vetting
# a) Bar
data_vetting_a <- data.frame(
  response = c("Yes:", "No:"),
  percentage = c(33,67)
)
pl_vetting_a <- do_barchart(data_vetting_a)

# b) How?
data_vetting_b <- data.frame(
  response = c("Output values\nare robust:\n", "First model\nperiod\ncompared\nto observed\ndata:"),
  percentage = c(50,50)
)
pl_vetting_b <- do_piechart(data_vetting_b)

# c) Why?
data_vetting_c <- data.frame(
  response = c("Lack\nof \ntime:\n", "Lack of\nknowladge:\n", "Lack of\nawareness\nof this\noption:",
               "Sectoral\nmodel:\n", "Other:\n"),
  percentage = c(12.5,25,25,12.5,25),
  cumulative_percentage = c(5,-7.5,9,-6,0)
)
pl_vetting_c <- do_piechart2(data_vetting_c, font_size = 8, offset_mm = 0.5)





##### - 5. Output storage (raw)
# a) Bar
data_rawStorage_a <- data.frame(
  response = c("Yes:", "No:"),
  percentage = c(33,67)
)
pl_rawStorage_a <- do_barchart(data_rawStorage_a)

# b) How?
data_rawStorage_b <- data.frame(
  response = c("Zenodo:"),
  percentage = c(100)
)
pl_rawStorage_b <- do_piechart(data_rawStorage_b)

# c) Why?
data_rawStorage_c <- data.frame(
  response = c("Lack of\ntime:\n", "Lack of\n knowladge:\n", "Lack of\n   awareness\n    of this\n         option:\n            ",
               "Too heavy\ndata:", "Other:\n"),
  percentage = c(22,34,11,22,11),
  cumulative_percentage = c(0,-14,0,-10,-5)
  # cumulative_percentage = c(-4,-3,-10,-4,-7)
)
pl_rawStorage_c <- do_piechart2(data_rawStorage_c, font_size = 8, offset_mm = 0.05, direction = "x")





##### - 6. Output storage (study)
# a) Bar
data_studyStorage_a <- data.frame(
  response = c("Yes:", "No:"),
  percentage = c(56,44)
)
pl_studyStorage_a <- do_barchart(data_studyStorage_a)

# b) How?
data_studyStorage_b <- data.frame(
  response = c("Zenodo:", "GitHub:\n"),
  percentage = c(83,17)
)
pl_studyStorage_b <- do_piechart(data_studyStorage_b, vjust_value = 0.58)

# c) Why?
data_studyStorage_c <- data.frame(
  response = c("Lack\nof time:\n", "Lack of\nknowladge:\n", "Lack of\nawareness\n   of this\n     option:\n         ", "Other:\n"),
  percentage = c(17,33,17,33),
  cumulative_percentage = c(0,-11,2,-5)
)
pl_studyStorage_c <- do_piechart2(data_studyStorage_c, font_size = 8,  offset_mm = 0.5)





##### - 7. Output visualization
# a) Bar
data_visualization_a <- data.frame(
  response = c("Yes:", "No:"),
  percentage = c(44,56)
)
pl_visualization_a <- do_barchart(data_visualization_a)

# b) How?
data_visualization_b <- data.frame(
  response = c("Model\nspecific\nplatform:\n    ",
               "Project specific\nplatform:",
               "Rshiny\n  widget:\n",
               "Other:\n"),
  percentage = c(16,52,16,16),
  cumulative_percentage = c(52,69,-10,45)
)
pl_visualization_b <- do_piechart2(data_visualization_b, font_size = 8,  offset_mm = 0.5)

# c) Why?
data_visualization_c <- data.frame(
  response = c("Lack of\ntime:\n", "Lack of\nknowladge:\n", "Lack of\nawareness\n   of this\n    option:\n     ", "Other:\n"),
  percentage = c(37.5,37.5,12.5,12.5),
  cumulative_percentage = c(-25,-34,-20,-30)
)
pl_visualization_c <- do_piechart2(data_visualization_c, font_size = 8,  offset_mm = 0.5)





##### - 8. Tools
# a) Bar
data_tools_a <- data.frame(
  response = c("Yes:", "No:"),
  percentage = c(22,88)
)
pl_tools_a <- do_barchart(data_tools_a)

# b) How?
data_tools_b <- data.frame(
  response = c("Zenodo:",
               "GitHub:",
               "GitLab:   \n",
               "Docker:\n",
               "Jupyter\nNote-\nbook:\n"),
  percentage = c(39.5,39.5,6,6,11),
  cumulative_percentage = c(-35,-20,31,-27,14)
)
pl_tools_b <- do_piechart2(data_tools_b, font_size = 8,  offset_mm = 1, direction = "both")

# c) Why?
data_tools_c <- data.frame(
  response = c("Data storage:\n", "Version\nrelease:\n", "Report\n& doc.\nsharing:", "Version control:\n", "Other:\n"),
  percentage = c(33,16,16,26,10),
  cumulative_percentage = c(28,32,4,25,47)
)
font_size = 8; offset_mm = 0.05; direction = "y"
pl_tools_c <-
  ggplot(data_tools_c, aes(x = "", y = percentage, fill = response)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text_repel(data = data_tools_c %>%
                    dplyr::filter(response != "Report\n& doc.\nsharing:") %>%
                    dplyr::mutate(cumulative_percentage = (cumulative_percentage + cumsum(percentage) - (percentage / 2) + 38) %% 100),
                  aes(label = dplyr::if_else(response == "", "",
                                             dplyr::if_else(substr(response, nchar(response), nchar(response)) != "\n",
                                                            paste0(response, " ", percentage, "%"),
                                                            paste0(response, percentage, "%"))),
                      y = cumulative_percentage),
                  size = font_size,
                  color = "black",
                  nudge_x = offset_mm / 25.4,
                  nudge_y = offset_mm / 25.4,
                  segment.size = 0,
                  direction = direction,
                  box.padding = 0,
                  point.padding = 0) +
  geom_text_repel(data = data_tools_c %>%
                    dplyr::filter(response == "Report\n& doc.\nsharing:") %>%
                    dplyr::mutate(cumulative_percentage = (cumulative_percentage + cumsum(percentage) - (percentage / 2) + 38) %% 100),
                  aes(label = dplyr::if_else(response == "", "",
                                             dplyr::if_else(substr(response, nchar(response), nchar(response)) != "\n",
                                                            paste0(response, " ", percentage, "%"),
                                                            paste0(response, percentage, "%"))),
                      y = cumulative_percentage),
                  size = font_size,
                  color = "black",
                  nudge_x = offset_mm / 25.4,
                  nudge_y = offset_mm / 25.4,
                  segment.size = 0,
                  direction = "both",
                  box.padding = 0,
                  point.padding = 0) +
  scale_fill_manual(values = colors.labels) +
  theme_void() +
  theme(legend.position = "none")



##### - Final figure
blank_p <- patchwork::plot_spacer() + theme_void()
data_harmonization_a <- data.frame(
  response = c("Yes", "No", "Does not\napply"),
  percentage = c(33,33,33)
)
data_harmonization_a$response <- factor(data_harmonization_a$response, levels = c('Yes','No','Does not\napply'))
legend <- ggpubr::get_legend(ggplot(data_harmonization_a, aes(x = 1, y = percentage, fill = response)) +
                               geom_bar(stat = "identity", width = 0.4) +
                               geom_text(aes(label = paste0(percentage, "%")),
                                         position = position_stack(vjust = 0.5),
                                         color = "black", size = 10) +
                               coord_flip() +
                               scale_fill_manual(values = colors.labels, name = 'Barchart\nlegend') +
                               theme_minimal() +
                               theme(
                                 axis.title = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 panel.grid = element_blank(),
                                 legend.direction = "vertical",
                                 legend.text = element_text(size = 25),
                                 legend.title = element_text(size = 30, face = "bold")
                               ) +
                               xlim(0.5, 1.5) +
                               guides(fill = guide_legend(keywidth = 4, keyheight = 4)))


pl <- cowplot::ggdraw() +
  ## 1. Model documentation
  cowplot::draw_plot_label(label = c("Is your model documented?"), size = 35, x = 0, y = 0.99) +
  cowplot::draw_plot_label(label = c("How?"), size = 30, x = 0.1, y = 0.925) +
  cowplot::draw_plot_label(label = c("Why not?"), size = 30, x = 0.22, y = 0.925) +
  cowplot::draw_plot(pl_documentation_a, x = 0, y = 0.9, width = 0.4, height = 0.1) +
  cowplot::draw_plot(pl_documentation_b, x = 0, y = 0.725, width = 0.2, height = 0.2) +
  cowplot::draw_plot(pl_documentation_c, x = 0.2, y = 0.725, width = 0.2, height = 0.2) +
  cowplot::draw_line(x = c(0.175, 0.1), y = c(0.925, 0.885),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  cowplot::draw_line(x = c(0.225, 0.3), y = c(0.925, 0.885),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  ## 2. Model harmonization & interconnections
  cowplot::draw_plot_label(label = c("Are your model harmonization &\n interconnections documented?"), size = 35, x = 0.475, y = 1.005) +
  cowplot::draw_plot_label(label = c("How?"), size = 30, x = 0.6, y = 0.925) +
  cowplot::draw_plot_label(label = c("Why not?"), size = 30, x = 0.72, y = 0.925) +
  cowplot::draw_plot(pl_harmonization_a, x = 0.5, y = 0.9, width = 0.4, height = 0.1) +
  cowplot::draw_plot(pl_harmonization_b, x = 0.5, y = 0.725, width = 0.2, height = 0.2) +
  cowplot::draw_plot(pl_harmonization_c, x = 0.7, y = 0.725, width = 0.2, height = 0.2) +
  cowplot::draw_line(x = c(0.675, 0.6), y = c(0.925, 0.885),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  cowplot::draw_line(x = c(0.725, 0.8), y = c(0.925, 0.885),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  ## 3. Output standardization
  cowplot::draw_plot_label(label = c("Do you standardize your model output?"), size = 35, x = -0.075, y = 0.75) +
  cowplot::draw_plot_label(label = c("Why not?"), size = 30, x = 0.22, y = 0.684) +
  cowplot::draw_plot(pl_standardization_a, x = 0, y = 0.66, width = 0.4, height = 0.1) +
  cowplot::draw_plot(pl_standardization_c, x = 0.2, y = 0.485, width = 0.2, height = 0.2) +
  cowplot::draw_line(x = c(0.225, 0.3), y = c(0.684, 0.645),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  ## 4. Output vetting
  cowplot::draw_plot_label(label = c("Do you vet your model output?"), size = 35, x = 0.49, y = 0.75) +
  cowplot::draw_plot_label(label = c("How?"), size = 30, x = 0.6, y = 0.684) +
  cowplot::draw_plot_label(label = c("Why not?"), size = 30, x = 0.72, y = 0.684) +
  cowplot::draw_plot(pl_vetting_a, x = 0.5, y = 0.66, width = 0.4, height = 0.1) +
  cowplot::draw_plot(pl_vetting_b, x = 0.5, y = 0.485, width = 0.2, height = 0.2) +
  cowplot::draw_plot(pl_vetting_c, x = 0.7, y = 0.485, width = 0.2, height = 0.2) +
  cowplot::draw_line(x = c(0.675, 0.6), y = c(0.684, 0.645),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  cowplot::draw_line(x = c(0.725, 0.8), y = c(0.684, 0.645),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  ## 5. Output storage (raw)
  cowplot::draw_plot_label(label = c("Do you store your raw outputs?"), size = 35, x = -0.0125, y = 0.5) +
  cowplot::draw_plot_label(label = c("How?"), size = 30, x = 0.1, y = 0.434) +
  cowplot::draw_plot_label(label = c("Why not?"), size = 30, x = 0.22, y = 0.434) +
  cowplot::draw_plot(pl_rawStorage_a, x = 0, y = 0.41, width = 0.4, height = 0.1) +
  cowplot::draw_plot(pl_rawStorage_b, x = 0, y = 0.235, width = 0.2, height = 0.2) +
  cowplot::draw_plot(pl_rawStorage_c, x = 0.2, y = 0.235, width = 0.2, height = 0.2) +
  cowplot::draw_line(x = c(0.175, 0.1), y = c(0.434, 0.395),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  cowplot::draw_line(x = c(0.225, 0.3), y = c(0.434, 0.395),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  ## 6. Output storage (study)
  cowplot::draw_plot_label(label = c("Do you store your study relevant outputs?"), size = 35, x = 0.415, y = 0.5) +
  cowplot::draw_plot_label(label = c("How?"), size = 30, x = 0.6, y = 0.434) +
  cowplot::draw_plot_label(label = c("Why not?"), size = 30, x = 0.72, y = 0.434) +
  cowplot::draw_plot(pl_studyStorage_a, x = 0.5, y = 0.41, width = 0.4, height = 0.1) +
  cowplot::draw_plot(pl_studyStorage_b, x = 0.5, y = 0.235, width = 0.2, height = 0.2) +
  cowplot::draw_plot(pl_studyStorage_c, x = 0.7, y = 0.235, width = 0.2, height = 0.2) +
  cowplot::draw_line(x = c(0.675, 0.6), y = c(0.434, 0.395),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  cowplot::draw_line(x = c(0.725, 0.8), y = c(0.434, 0.395),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  ## 7. Output visualization
  cowplot::draw_plot_label(label = c("Do you present your model outputs through\ninteractive user-friendly visualization tools?"), size = 35, x = -0.11, y = 0.26) +
  cowplot::draw_plot_label(label = c("How?"), size = 30, x = 0.1, y = 0.174) +
  cowplot::draw_plot_label(label = c("Why not?"), size = 30, x = 0.22, y = 0.174) +
  cowplot::draw_plot(pl_visualization_a, x = 0, y = 0.15, width = 0.4, height = 0.1) +
  cowplot::draw_plot(pl_visualization_b, x = 0, y = -0.025, width = 0.2, height = 0.2) +
  cowplot::draw_plot(pl_visualization_c, x = 0.2, y = -0.025, width = 0.2, height = 0.2) +
  cowplot::draw_line(x = c(0.175, 0.1), y = c(0.174, 0.135),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  cowplot::draw_line(x = c(0.225, 0.3), y = c(0.174, 0.135),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  ## 8. Tools
  cowplot::draw_plot_label(label = c("Did you create any open-source tool(s) to facilitate\n    the implementation of open science practices?"), size = 35, x = 0.35, y = 0.26) +
  cowplot::draw_plot_label(label = c("Which open-source tools do you use\nto follow open science practices?"), size = 27.5, x = 0.34, y = 0.18) +
  cowplot::draw_plot_label(label = c("Why do you use open-source tools?"), size = 27.5, x = 0.65, y = 0.174) +
  cowplot::draw_plot(pl_tools_a, x = 0.5, y = 0.15, width = 0.4, height = 0.1) +
  cowplot::draw_plot(pl_tools_b, x = 0.5, y = -0.025, width = 0.2, height = 0.2) +
  cowplot::draw_plot(pl_tools_c, x = 0.7, y = -0.025, width = 0.2, height = 0.2) +
  cowplot::draw_line(x = c(0.675, 0.6), y = c(0.174, 0.135),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  cowplot::draw_line(x = c(0.725, 0.8), y = c(0.174, 0.135),
                     arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed"),
                     color = "black", size = 2) +
  ## legend
  cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,ncol=1), x = 0.45, y = -0.225, width = 1, height = 1) +
  ## labels
  cowplot::draw_plot_label(label = c("a)", "b)", "c)", "d)",
                                     "e)", "f)", "g)", "h)"),
                           size = 30,
                           x = c(0, 0.5,
                                 0, 0.5,
                                 0, 0.5,
                                 0, 0.5),
                           y = c(0.99, 0.99,
                                 0.75, 0.75,
                                 0.5, 0.5,
                                 0.25, 0.25))


ggsave(file='Survey_responses.pdf', plot = pl, width = 840, height = 1188, unit = 'mm') #A4 dimensions


