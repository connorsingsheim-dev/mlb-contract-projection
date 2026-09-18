library(shiny)
library(tidyverse)
library(plotly)

# Load final datasets

batting <- read_csv(
  "Batting_Contracts.csv",
  show_col_types = FALSE
)

pitching <- read_csv(
  "Pitching_Contracts.csv",
  show_col_types = FALSE
)

# Group hitters into broader position groups

batting <- batting |>
  mutate(
    Position_Group = case_when(
      str_detect(position, "C") ~ "C",
      str_detect(position, "OF") | str_detect(position, "DH") ~ "OF",
      TRUE ~ "INF"
    )
  )

# Latest season

latest_season <- 2025

# Current players for dropdowns

current_batters <- batting |>
  filter(
    Season == latest_season,
    PA > 0
  ) |>
  distinct(playerid, .keep_all = TRUE) |>
  arrange(PlayerName)

current_pitchers <- pitching |>
  filter(Season == latest_season) |>
  distinct(playerid, .keep_all = TRUE) |>
  arrange(PlayerName)

# Format contract values

money_format <- function(x) {
  
  case_when(
    is.na(x) ~ "",
    
    x >= 1000000 ~ paste0(
      "$",
      round(x / 1000000, 1),
      "M"
    ),
    
    TRUE ~ paste0(
      "$",
      format(
        round(x),
        big.mark = ",",
        scientific = FALSE
      )
    )
  )
}

# Make PCA axis labels easier to understand

pc_label <- function(pca, pc_number) {
  
  loadings <- abs(
    pca$rotation[, pc_number]
  )
  
  top_stats <- names(
    sort(
      loadings,
      decreasing = TRUE
    )
  )[1:3]
  
  paste0(
    "Player Profile ",
    pc_number,
    " (",
    paste(top_stats, collapse = ", "),
    ")"
  )
}


ui <- fluidPage(
  
  titlePanel("MLB Free Agent Contract Comparables"),
  
  tabsetPanel(
    
    # Batter tab
    
    tabPanel(
      "Batter Contract Comps",
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectizeInput(
            "batter_player",
            "Select 2025 Batter:",
            choices = current_batters$PlayerName,
            selected = current_batters$PlayerName[1]
          ),
          
          sliderInput(
            "batter_age",
            "Historical Comp Age Range:",
            min = 18,
            max = 45,
            value = c(24, 32)
          ),
          
          numericInput(
            "batter_clusters",
            "Number of Clusters:",
            value = 3,
            min = 2,
            max = 8,
            step = 1
          ),
          
          hr(),
          
          h4("Projected Contract"),
          
          uiOutput("batter_projection")
        ),
        
        mainPanel(
          
          h3("Statistical Clusters"),
          
          plotlyOutput(
            "batter_plot",
            height = "500px"
          ),
          
          tags$small(
            "Clusters are based on the full statistical profile. The graph displays the first two principal components as a simplified two dimensional view."
          ),
          
          hr(),
          
          h3("Closest Free Agent Comparables"),
          
          tableOutput("batter_table")
        )
      )
    ),
    
    
    # Pitcher tab
    
    tabPanel(
      "Pitcher Contract Comps",
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectizeInput(
            "pitcher_player",
            "Select 2025 Pitcher:",
            choices = current_pitchers$PlayerName,
            selected = current_pitchers$PlayerName[1]
          ),
          
          sliderInput(
            "pitcher_age",
            "Historical Comp Age Range:",
            min = 18,
            max = 45,
            value = c(24, 32)
          ),
          
          numericInput(
            "pitcher_clusters",
            "Number of Clusters:",
            value = 3,
            min = 2,
            max = 8,
            step = 1
          ),
          
          hr(),
          
          h4("Projected Contract"),
          
          uiOutput("pitcher_projection")
        ),
        
        mainPanel(
          
          h3("Statistical Clusters"),
          
          plotlyOutput(
            "pitcher_plot",
            height = "500px"
          ),
          
          tags$small(
            "Clusters are based on the full statistical profile. The graph displays the first two principal components as a simplified two dimensional view."
          ),
          
          hr(),
          
          h3("Closest Free Agent Comparables"),
          
          tableOutput("pitcher_table")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Update batter age range
  
  observeEvent(input$batter_player, {
    
    selected_age <- current_batters |>
      filter(PlayerName == input$batter_player) |>
      pull(Age) |>
      first()
    
    if (!is.na(selected_age)) {
      
      updateSliderInput(
        session,
        "batter_age",
        value = c(
          max(18, selected_age - 3),
          min(45, selected_age + 3)
        )
      )
    }
  })
  
  
  # Update pitcher age range
  
  observeEvent(input$pitcher_player, {
    
    selected_age <- current_pitchers |>
      filter(PlayerName == input$pitcher_player) |>
      pull(Age) |>
      first()
    
    if (!is.na(selected_age)) {
      
      updateSliderInput(
        session,
        "pitcher_age",
        value = c(
          max(18, selected_age - 3),
          min(45, selected_age + 3)
        )
      )
    }
  })
  
  
  # Batter clustering
  
  batter_results <- reactive({
    
    req(
      input$batter_player,
      input$batter_age,
      input$batter_clusters
    )
    
    selected_player <- current_batters |>
      filter(PlayerName == input$batter_player) |>
      slice(1)
    
    selected_position <- selected_player$Position_Group[1]
    
    historical <- batting |>
      filter(
        Season >= 2020,
        Season < latest_season,
        !is.na(Total_Salary),
        Position_Group == selected_position,
        Age >= input$batter_age[1],
        Age <= input$batter_age[2]
      )
    
    cluster_data <- bind_rows(
      historical,
      selected_player
    ) |>
      select(
        playerid,
        PlayerName,
        Season,
        Age,
        position,
        Position_Group,
        WAR,
        PA,
        HR,
        BB,
        SO,
        SB,
        AVG,
        OBP,
        SLG,
        OPS,
        wRC_plus,
        Signing_Season,
        `Signing Team`,
        Years,
        Total_Salary,
        Contract_AAV
      ) |>
      drop_na(
        WAR,
        PA,
        HR,
        BB,
        SO,
        AVG,
        OBP,
        SLG,
        OPS,
        wRC_plus
      )
    
    if (nrow(cluster_data) <= input$batter_clusters) {
      return(NULL)
    }
    
    stats <- cluster_data |>
      select(
        WAR,
        PA,
        HR,
        BB,
        SO,
        SB,
        AVG,
        OBP,
        SLG,
        OPS,
        wRC_plus
      )
    
    scaled_stats <- scale(stats)
    
    set.seed(123)
    
    km <- kmeans(
      scaled_stats,
      centers = input$batter_clusters,
      nstart = 25
    )
    
    pca <- prcomp(
      scaled_stats,
      center = FALSE,
      scale. = FALSE
    )
    
    pc1_label <- pc_label(
      pca,
      1
    )
    
    pc2_label <- pc_label(
      pca,
      2
    )
    
    results <- cluster_data |>
      mutate(
        Cluster = km$cluster,
        PC1 = pca$x[, 1],
        PC2 = pca$x[, 2],
        Selected = (
          playerid == selected_player$playerid[1] &
            Season == latest_season
        )
      )
    
    selected_index <- which(
      results$Selected
    )[1]
    
    if (is.na(selected_index)) {
      return(NULL)
    }
    
    selected_scaled <- scaled_stats[
      selected_index,
    ]
    
    results$Distance <- sqrt(
      rowSums(
        sweep(
          scaled_stats,
          2,
          selected_scaled
        )^2
      )
    )
    
    selected_cluster <- results$Cluster[
      selected_index
    ]
    
    # Closest players ranked by full statistical distance
    
    comps <- results |>
      filter(
        !Selected,
        !is.na(Total_Salary)
      ) |>
      arrange(Distance)
    
    list(
      data = results,
      comps = comps,
      selected_cluster = selected_cluster,
      position = selected_position,
      pc1_label = pc1_label,
      pc2_label = pc2_label
    )
  })
  
  
  # Pitcher clustering
  
  pitcher_results <- reactive({
    
    req(
      input$pitcher_player,
      input$pitcher_age,
      input$pitcher_clusters
    )
    
    selected_player <- current_pitchers |>
      filter(PlayerName == input$pitcher_player) |>
      slice(1)
    
    historical <- pitching |>
      filter(
        Season >= 2020,
        Season < latest_season,
        !is.na(Total_Salary),
        Age >= input$pitcher_age[1],
        Age <= input$pitcher_age[2]
      )
    
    cluster_data <- bind_rows(
      historical,
      selected_player
    ) |>
      select(
        playerid,
        PlayerName,
        Season,
        Age,
        position,
        WAR,
        IP,
        ERA,
        K_9,
        BB_9,
        K_BB,
        H_9,
        HR_9,
        WHIP,
        FIP,
        Signing_Season,
        `Signing Team`,
        Years,
        Total_Salary,
        Contract_AAV
      ) |>
      drop_na(
        WAR,
        IP,
        ERA,
        K_9,
        BB_9,
        K_BB,
        H_9,
        HR_9,
        WHIP,
        FIP
      )
    
    if (nrow(cluster_data) <= input$pitcher_clusters) {
      return(NULL)
    }
    
    stats <- cluster_data |>
      select(
        WAR,
        IP,
        ERA,
        K_9,
        BB_9,
        K_BB,
        H_9,
        HR_9,
        WHIP,
        FIP
      )
    
    scaled_stats <- scale(stats)
    
    set.seed(123)
    
    km <- kmeans(
      scaled_stats,
      centers = input$pitcher_clusters,
      nstart = 25
    )
    
    pca <- prcomp(
      scaled_stats,
      center = FALSE,
      scale. = FALSE
    )
    
    pc1_label <- pc_label(
      pca,
      1
    )
    
    pc2_label <- pc_label(
      pca,
      2
    )
    
    results <- cluster_data |>
      mutate(
        Cluster = km$cluster,
        PC1 = pca$x[, 1],
        PC2 = pca$x[, 2],
        Selected = (
          playerid == selected_player$playerid[1] &
            Season == latest_season
        )
      )
    
    selected_index <- which(
      results$Selected
    )[1]
    
    if (is.na(selected_index)) {
      return(NULL)
    }
    
    selected_scaled <- scaled_stats[
      selected_index,
    ]
    
    results$Distance <- sqrt(
      rowSums(
        sweep(
          scaled_stats,
          2,
          selected_scaled
        )^2
      )
    )
    
    selected_cluster <- results$Cluster[
      selected_index
    ]
    
    # Closest pitchers ranked by full statistical distance
    
    comps <- results |>
      filter(
        !Selected,
        !is.na(Total_Salary)
      ) |>
      arrange(Distance)
    
    list(
      data = results,
      comps = comps,
      selected_cluster = selected_cluster,
      pc1_label = pc1_label,
      pc2_label = pc2_label
    )
  })
  
  
  # Batter plot
  
  output$batter_plot <- renderPlotly({
    
    current <- batter_results()
    
    if (is.null(current)) {
      return(NULL)
    }
    
    graph_data <- current$data |>
      mutate(
        Cluster_Label = factor(
          paste(
            "Cluster",
            Cluster
          ),
          levels = paste(
            "Cluster",
            sort(unique(Cluster))
          )
        ),
        
        Hover = paste0(
          "<b>",
          PlayerName,
          "</b>",
          "<br>Season: ",
          Season,
          "<br>Age: ",
          Age,
          "<br>Cluster: ",
          Cluster,
          "<br>WAR: ",
          round(WAR, 2),
          "<br>PA: ",
          PA,
          "<br>HR: ",
          HR,
          "<br>OPS: ",
          round(OPS, 3),
          "<br>wRC+: ",
          round(wRC_plus, 0),
          
          ifelse(
            !is.na(Total_Salary),
            paste0(
              "<br>Contract: ",
              money_format(Total_Salary)
            ),
            ""
          )
        )
      )
    
    p <- ggplot(
      graph_data,
      aes(
        x = PC1,
        y = PC2,
        color = Cluster_Label,
        text = Hover
      )
    ) +
      
      geom_point(
        size = 3,
        alpha = 0.8
      ) +
      
      # Black ring around selected player
      
      geom_point(
        data = graph_data |>
          filter(Selected),
        aes(
          x = PC1,
          y = PC2,
          shape = "Selected Player (2025)",
          text = Hover
        ),
        inherit.aes = FALSE,
        size = 6,
        fill = NA,
        color = "black",
        stroke = 1.8,
        show.legend = TRUE
      ) +
      
      scale_shape_manual(
        name = NULL,
        values = c(
          "Selected Player (2025)" = 21
        )
      ) +
      
      labs(
        title = paste(
          input$batter_clusters,
          "Batter Clusters"
        ),
        
        subtitle = paste(
          current$position,
          "players | Historical comp age",
          input$batter_age[1],
          "to",
          input$batter_age[2]
        ),
        
        x = current$pc1_label,
        y = current$pc2_label,
        color = "Cluster"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        legend.position = "right",
        plot.title = element_text(
          face = "bold"
        )
      )
    
    plotly_graph <- ggplotly(
      p,
      tooltip = "text"
    )
    
    plotly_graph$x$data <- lapply(
      plotly_graph$x$data,
      function(trace) {
        
        if (!is.null(trace$name)) {
          
          trace$name <- str_replace(
            trace$name,
            "^\\(Cluster ([0-9]+),1\\)$",
            "Cluster \\1"
          )
          
          trace$name <- str_replace(
            trace$name,
            "^\\(Selected Player \\(2025\\),1\\)$",
            "Selected Player (2025)"
          )
        }
        
        trace
      }
    )
    
    plotly_graph
  })
  
  
  # Pitcher plot
  
  output$pitcher_plot <- renderPlotly({
    
    current <- pitcher_results()
    
    if (is.null(current)) {
      return(NULL)
    }
    
    graph_data <- current$data |>
      mutate(
        Cluster_Label = factor(
          paste(
            "Cluster",
            Cluster
          ),
          levels = paste(
            "Cluster",
            sort(unique(Cluster))
          )
        ),
        
        Hover = paste0(
          "<b>",
          PlayerName,
          "</b>",
          "<br>Season: ",
          Season,
          "<br>Age: ",
          Age,
          "<br>Cluster: ",
          Cluster,
          "<br>WAR: ",
          round(WAR, 2),
          "<br>IP: ",
          round(IP, 1),
          "<br>ERA: ",
          round(ERA, 2),
          "<br>K/9: ",
          round(K_9, 2),
          "<br>BB/9: ",
          round(BB_9, 2),
          "<br>FIP: ",
          round(FIP, 2),
          
          ifelse(
            !is.na(Total_Salary),
            paste0(
              "<br>Contract: ",
              money_format(Total_Salary)
            ),
            ""
          )
        )
      )
    
    p <- ggplot(
      graph_data,
      aes(
        x = PC1,
        y = PC2,
        color = Cluster_Label,
        text = Hover
      )
    ) +
      
      geom_point(
        size = 3,
        alpha = 0.8
      ) +
      
      # Black ring around selected player
      
      geom_point(
        data = graph_data |>
          filter(Selected),
        aes(
          x = PC1,
          y = PC2,
          shape = "Selected Player (2025)",
          text = Hover
        ),
        inherit.aes = FALSE,
        size = 6,
        fill = NA,
        color = "black",
        stroke = 1.8,
        show.legend = TRUE
      ) +
      
      scale_shape_manual(
        name = NULL,
        values = c(
          "Selected Player (2025)" = 21
        )
      ) +
      
      labs(
        title = paste(
          input$pitcher_clusters,
          "Pitcher Clusters"
        ),
        
        subtitle = paste(
          "Historical comp age",
          input$pitcher_age[1],
          "to",
          input$pitcher_age[2]
        ),
        
        x = current$pc1_label,
        y = current$pc2_label,
        color = "Cluster"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        legend.position = "right",
        plot.title = element_text(
          face = "bold"
        )
      )
    
    plotly_graph <- ggplotly(
      p,
      tooltip = "text"
    )
    
    plotly_graph$x$data <- lapply(
      plotly_graph$x$data,
      function(trace) {
        
        if (!is.null(trace$name)) {
          
          trace$name <- str_replace(
            trace$name,
            "^\\(Cluster ([0-9]+),1\\)$",
            "Cluster \\1"
          )
          
          trace$name <- str_replace(
            trace$name,
            "^\\(Selected Player \\(2025\\),1\\)$",
            "Selected Player (2025)"
          )
        }
        
        trace
      }
    )
    
    plotly_graph
  })
  
  
  # Batter table
  
  output$batter_table <- renderTable({
    
    current <- batter_results()
    
    if (is.null(current)) {
      return(NULL)
    }
    
    current$comps |>
      slice_head(n = 10) |>
      transmute(
        Player = PlayerName,
        Season,
        Age,
        Cluster,
        WAR = round(WAR, 2),
        PA,
        HR,
        OPS = round(OPS, 3),
        `wRC+` = round(wRC_plus, 0),
        `Signing Team`,
        Years,
        `Total Contract` = money_format(
          Total_Salary
        ),
        AAV = money_format(
          Contract_AAV
        ),
        Distance = round(
          Distance,
          2
        )
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # Pitcher table
  
  output$pitcher_table <- renderTable({
    
    current <- pitcher_results()
    
    if (is.null(current)) {
      return(NULL)
    }
    
    current$comps |>
      slice_head(n = 10) |>
      transmute(
        Player = PlayerName,
        Season,
        Age,
        Cluster,
        WAR = round(WAR, 2),
        IP = round(IP, 1),
        ERA = round(ERA, 2),
        `K/9` = round(K_9, 2),
        `BB/9` = round(BB_9, 2),
        FIP = round(FIP, 2),
        `Signing Team`,
        Years,
        `Total Contract` = money_format(
          Total_Salary
        ),
        AAV = money_format(
          Contract_AAV
        ),
        Distance = round(
          Distance,
          2
        )
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # Batter projection
  
  output$batter_projection <- renderUI({
    
    current <- batter_results()
    
    if (
      is.null(current) ||
      nrow(current$comps) == 0
    ) {
      
      return(
        p(
          "No contract comps available."
        )
      )
    }
    
    projection_comps <- current$comps |>
      slice_head(n = 5)
    
    projected_years <- median(
      projection_comps$Years,
      na.rm = TRUE
    )
    
    projected_total <- median(
      projection_comps$Total_Salary,
      na.rm = TRUE
    )
    
    projected_aav <- median(
      projection_comps$Contract_AAV,
      na.rm = TRUE
    )
    
    tagList(
      
      h4(
        paste0(
          round(
            projected_years,
            1
          ),
          " Years"
        )
      ),
      
      h4(
        money_format(
          projected_total
        )
      ),
      
      p(
        paste(
          "Projected AAV:",
          money_format(
            projected_aav
          )
        )
      ),
      
      tags$small(
        "Projection based on the five closest free agent comparables."
      )
    )
  })
  
  
  # Pitcher projection
  
  output$pitcher_projection <- renderUI({
    
    current <- pitcher_results()
    
    if (
      is.null(current) ||
      nrow(current$comps) == 0
    ) {
      
      return(
        p(
          "No contract comps available."
        )
      )
    }
    
    projection_comps <- current$comps |>
      slice_head(n = 5)
    
    projected_years <- median(
      projection_comps$Years,
      na.rm = TRUE
    )
    
    projected_total <- median(
      projection_comps$Total_Salary,
      na.rm = TRUE
    )
    
    projected_aav <- median(
      projection_comps$Contract_AAV,
      na.rm = TRUE
    )
    
    tagList(
      
      h4(
        paste0(
          round(
            projected_years,
            1
          ),
          " Years"
        )
      ),
      
      h4(
        money_format(
          projected_total
        )
      ),
      
      p(
        paste(
          "Projected AAV:",
          money_format(
            projected_aav
          )
        )
      ),
      
      tags$small(
        "Projection based on the five closest free agent comparables."
      )
    )
  })
}


shinyApp(
  ui = ui,
  server = server
)