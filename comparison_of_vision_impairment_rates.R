graphics.off()
rm(list=ls())
library(readxl)
library(dplyr)
library(plotly)
library(viridis)
library(unikn)

## Set color palette
vir_5 <- viridis(n = 5)
vision_loss_colors <- vir_5 

## Load data
setwd("C:/Users/Admin/Downloads")
vision_data <- read_excel("Vision loss global 2020_prevalance.xlsx", sheet = "Sheet1")
head(vision_data,5)

## Tidy
vision_data$Prevalence <- vision_data$Prevalence*100
vision_data$`Vision Loss Type` <- factor(vision_data$`Vision Loss Type`, 
                                         levels = c("All vision loss", "Blindness", "Mild", "Moderate to severe", "Near"),
                                         ordered = TRUE)

# Separate Australia vs World and calculate prevalence
vision_summary <- vision_data %>%
  mutate(Location = ifelse(`Country Name` == "Australia", "Australia", "World")) %>%
  group_by(Location, Gender, `Vision Loss Type`) %>%
  summarise(Average_Prevalence = sum(`Number affected`, na.rm = TRUE) / sum(Population, na.rm = TRUE) * 100,
            Affected_People = sum(`Number affected`, na.rm = TRUE),
            .groups = 'drop')

## Tooltip
vision_summary <- vision_summary %>%
  mutate(Tooltip = paste(Location,
                         "<br>Prevalence:", round(Average_Prevalence, 2), "%",
                         "<br>Number affected:", format(Affected_People, big.mark = ","),
                         "<br>Vision Loss Type:", `Vision Loss Type`))

# Reverse the x-axis direction for female
vision_summary <- vision_summary %>%
  mutate(Average_Prevalence = ifelse(Location == "Australia", -Average_Prevalence, Average_Prevalence))

# Add space for y-axis labels
vision_summary$`Vision Loss Type` <- paste0( vision_summary$`Vision Loss Type`,"    ")


## Individual plots for each facet
plot_list <- list()
for (loc in unique(vision_summary$Location)) {
  for (gender in unique(vision_summary$Gender)) {
    subset_data <- vision_summary %>% filter(Location == loc, Gender == gender)
    
    plot_list[[paste(loc, gender)]] <- plot_ly(
      data = subset_data,
      x = ~Average_Prevalence,
      y = ~`Vision Loss Type`,  
      type = "bar",
      orientation = 'h', # horizontal bar charts
      text = ~Tooltip,
      hoverinfo = "text",
      color = ~`Vision Loss Type`,
      colors = vision_loss_colors,
      legendgroup = ~`Vision Loss Type`,  # Ensure legend works for all
      textposition = "none", # Hide text directly on the plot
      showlegend = (loc == "Australia" && gender == "Male")  # Show first plot's legend only
    ) %>%
      layout(
        title = list(text = paste(loc, "-", gender), x = 0.5),
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(title = "")
      )
  }
}

## Arrange plots in facet wrap
final_plot <- subplot(
  plot_list[["Australia Male"]], plot_list[["World Male"]],
  plot_list[["Australia Female"]], plot_list[["World Female"]],
  nrows = 2, shareY = TRUE, titleX = TRUE
) %>%
  layout(
    # Main title
    title = list(
      text = "<b>Comparison of Vision Impairment Rates: Australia vs. World</b>",
      font = list(size = 25, family = "Arial"),
      x = 0.598, xanchor = "right", y = 0.94),
    # Legend
    legend = list(
      orientation = "h", x = 0.5, xanchor = "center", y = -0.16,
      font = list(size = 14)),
    margin = list(t = 150, b = 20, r = 130, l = 150),  
    bargap = 0.01,
    # X-axis label, subtitle & Source
    annotations = list(
      list(
        x = 0.5, y = -0.09, text = "Prevalence (%)",  # Center horizontally
        showarrow = FALSE, xref = "paper", yref = "paper",
        font = list(size = 14), xanchor = "center", yanchor = "top"
      ),
      list(
        x = 0.452, y = 1.268, text = "An overview of vision impairment by gender and type in 2020",
        showarrow = FALSE, xref = "paper", yref = "paper",
        font = list(size = 18), xanchor = "right", yanchor = "top"
      ),
      list(
        x = 0.6, y = -0.28, text = "Source: Data from VLEG/GBD 2020 model, accessed via the IAPB Vision Atlas",
        showarrow = FALSE, xref = "paper", yref = "paper",
        font = list(size = 12, color = "gray"), xanchor = "left", yanchor = "bottom"
      ),
      # Gender and region titles
      list(
        x = 0.25, y = 1.13, text = "<b>Australia</b>", showarrow = FALSE,
        xref = "paper", yref = "paper", font = list(size = 14), xanchor = "center"
      ),
      list(
        x = 0.75, y = 1.13, text = "<b>World</b>", showarrow = FALSE,
        xref = "paper", yref = "paper", font = list(size = 14), xanchor = "center"
      ),
      list(
        x = 1.08, y = 0.75, text = "<b>Male</b>", showarrow = FALSE,
        xref = "paper", yref = "paper", font = list(size = 14), yanchor = "left", textangle = 0
      ),
      list(
        x = 1.1, y = 0.25, text = "<b>Female</b>", showarrow = FALSE,
        xref = "paper", yref = "paper", font = list(size = 14), yanchor = "left", textangle = 0
      ),
      # Alt text
      list(
        x = -0.1, y = 1.2, text = "ℹ️", xref = "paper", yref = "paper", showarrow = FALSE,
        font = list(size = 20, color = "blue"),
        hovertext = "This bar chart, titled ‘Comparison of Vision Impairment Rates: Australia vs. World,’ compares vision impairment types by gender in 2020. It’s divided into four sections: Australia Male, World <br> Male, Australia Female, and World Female. Each section displays horizontal bars for five impairment types: ‘All vision loss,’ ‘Blindness,’ ‘Mild,’ ‘Moderate to severe,’ and ‘Near.’ The x-axis <br> represents prevalence across these categories. Globally, ‘All vision loss’ affects 7.7% of females and 6.3% of males, significantly higher than Australia’s rates of 2.84% and 2.32%, <br>respectively. ‘Near’ vision impairment also shows a marked difference, impacting 3.5% of females and 2.9% of males worldwide, compared to 0.63% and 0.46% in Australia. ‘Mild’ and <br> ‘Moderate to severe’ impairments follow a similar trend, with global rates almost double those in Australia. ‘Blindness’ remains rare across the board but is still slightly more common <br> globally. Overall, vision impairment prevalence is notably higher worldwide than in Australia across all categories.",
        hoverlabel = list(bgcolor = "white", font = list(size = 12))
      )
    ),
    # X-axis options
    xaxis = list(showgrid = TRUE, title = "", showticklabels = FALSE, range = c(-8, 0),
                 tickvals = seq(-8, 0, by = 1),
                 ticktext = as.character(seq(8, 0, by = -1))),
    xaxis2 = list(showgrid = TRUE, title = "", showticklabels = FALSE, range = c(0, 8),
                  tickvals = seq(0, 8, by = 1),
                  ticktext = as.character(seq(0, 8, by = 1))),
    xaxis3 = list(showgrid = TRUE, title = "", range = c(-8, 0),
                  tickvals = seq(-8, 0, by = 1),
                  ticktext = as.character(seq(8, 0, by = -1))),
    xaxis4 = list(showgrid = TRUE, title = "", range = c(0, 8),
                  tickvals = seq(0, 8, by = 1),
                  ticktext = as.character(seq(0, 8, by = 1))),
    
    # Y-axis options
    yaxis = list(showgrid = TRUE, title = ""),
    yaxis2 = list(showgrid = TRUE, title = ""),
    yaxis3 = list(showgrid = TRUE, title = ""),
    yaxis4 = list(showgrid = TRUE, title = "")
  )

# final_plot
## Save as html file
htmlwidgets::saveWidget(final_plot, "vision_impairment_plot.html")
