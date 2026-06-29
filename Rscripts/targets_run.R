library(patchwork)
targets::tar_make()
targets::tar_meta(fields = warnings, complete_only = TRUE) 
summary_table <- targets::tar_read(convergence_summary)

summary_table |>
  dplyr::slice_min(biomass_error, n = 1, with_ties = TRUE)
summary_table |>
  dplyr::slice_min(recruitment_error, n = 1, with_ties = TRUE)
summary_table |>
  dplyr::slice_min(fmort_error, n = 1, with_ties = TRUE)

a <- summary_table |>
  dplyr::mutate(
    biomass_z = scale(biomass_error)[,1],
    recruitment_z = scale(recruitment_error)[,1],
    fmort_z = scale(fmort_error)[,1],
    avg_z_error = rowMeans(
      cbind(biomass_z, recruitment_z, fmort_z),
      na.rm = TRUE
    )
  ) |> 
  dplyr::slice_min(avg_z_error, n = 1, with_ties = TRUE)
a
scenario_id <- 15
all_fits <- targets::tar_read(fims_fits)
all_fits[[scenario_id]][["estimates"]] |>
  dplyr::filter(estimation_type == "fixed_effects" | estimation_type == "random_effects") |>
  dplyr::select(module_name, label, fleet, year_i, age_i, input, estimated, uncertainty) |>
  print(n = Inf)

# Pull all the generated plots out of the targets cache
all_plots <- targets::tar_read(scenario_plots)
all_plots[[25]]
valid_plots <- purrr::compact(all_plots)
scenario_pages <- purrr::imap(valid_plots, function(plot_list, index) {
  
  # Extract and individually title each plot
  p_bio <- plot_list[["biomass"]] + ggplot2::ggtitle("Biomass")
  p_rec <- plot_list[["recruitment"]] + ggplot2::ggtitle("Recruitment")
  p_f   <- plot_list[["fmort"]] + ggplot2::ggtitle("Fishing Mortality")
  
  # Arrange them: Biomass on top, Recruitment and F side-by-side on the bottom
  combined_plot <- (p_bio / (p_rec | p_f)) + 
    patchwork::plot_annotation(
      title = paste("Diagnostics for Scenario", index),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  return(combined_plot)
})

grDevices::pdf("all_scenarios_figures.pdf", width = 11, height = 8.5)
purrr::walk(scenario_pages, print)
grDevices::dev.off()

targets::tar_visnetwork()
