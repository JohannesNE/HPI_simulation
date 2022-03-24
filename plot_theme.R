# Plot theme
# Theme for plot
theme_set(
    theme_minimal(base_size = 11) +
        theme(
            plot.title = ggtext::element_textbox_simple(
                size = 10,
                vjust = 0,
                minheight = unit(3, "lines"), width = unit(2, "inches")
            ),
            axis.title = element_text(size = 9.5),
            axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
            panel.grid.major = ggplot2::element_line(
                color = "#454545",
                size = 0.3,
                linetype = "dotted"
            ),
            panel.grid.minor = ggplot2::element_blank()
        ))

plot_colors <- c("#2A6EBB", "#bb612a")
