#' Create all graph images of a data point using common y-axes
#'
#' @param column A variable name
#' @return A data frame of all data points found in the graphs
#' @export
create_images <- function(column, value, df = "", map_dfs = "", upload_graphs = F, upload_data = F) {
  
  # Create graphs by variable or data frame
  for(v in graph_specs$variable[graph_specs[[column]] %in% value]){
    output <- graph(v, df, map_dfs, upload = upload_graphs)
    graph_data <- assign_col_join(graph_data, output, by = c("year", "category", "variable"))
  }
  
  graph_data %<>% clean_graph_df()
  
  if (upload_data) {
    filename   <- value %p% ".csv"
    drive_path <- paste0("Website/Images/", value, "/", filename)
    file_path  <- paste0("image_data/", filename)
    
    write_csv(graph_data, file_path)
    
    if(filename %in% googledrive::drive_ls("Website/Data")$name) {
      response <- googledrive::drive_update(drive_path, file_path, verbose = F)
    } else {
      response <- googledrive::drive_upload(file_path, drive_path, verbose = F)
    }
  }
  
  graph_data
  
}

#' Create all graph images of a data point using common y-axes
#'
#' @param var A variable name
#' @return A data frame of all data points found in the graphs
#' @export
clean_graph_df <- function(df) {
  df %<>% organize()
  
  unincluded <- df %cols_not_in% graph_specs$variable
  unincluded <- unincluded[unincluded %not_in% c("year", "category", "variable")]
  
  print("Variables not included:" %p% unincluded)
  
  df %>%
    mutate_at(df %cols_in% graph_specs$variable[graph_specs$units %in% c("Percent", "Score", "Ratio")], comma, accuracy = 0.1) %>%
    mutate_at(df %cols_in% graph_specs$variable[graph_specs$units == "Dollars"], comma, accuracy = 100) %>%
    purrr::map_df(~ replace(., . %in% c("NA", "NaN"), ""))
}

#' Create all graph images of a data point using common y-axes
#'
#' @param var A variable name
#' @return A data frame of all data points found in the graphs
#' @export
graph <- function(var, df = "", map_dfs = "", save_image = T, return_ylims = F, upload = F, path = "", ranking = TRUE, transparent = TRUE) {
  print(var)

  # Get graph specs and data frame
  g <- graph_specs %>% filter(variable == var)
  if (typeof(df) == "character") df <- get(paste0(g$data_frame, "_", g$geography))

  if ("var_type" %in% names(df)) df %<>% filter(var_type == "percent")

  # Calculate y-axis limits for the combined graphs
  get_y_lims(df, var) %>%
    list2env(envir = parent.env(environment()))

  # Create folder if one does not exist
  if (path == "") path <- paste0("output_images/", g$folder)
  if (!dir.exists(path)) dir.create(path)

  path <- path %p% "/"

  # Create graphs
  if (df_type(df) %in% c("FIPS", "MSA") & ranking)  graph_ranking(var, df, save_image, upload, g$file_prefix, transparent)
                                                    graph_trendline(var, df, ylims, save_image, upload, g$file_prefix, transparent)
  if (df_type(df) %in% c("FIPS", "MSA"))            graph_maxmin(var, df, ylims, save_image, upload, g$file_prefix, transparent)
  if (g$sex)                                        graph_sex(var, df, ylims, save_image, upload, g$file_prefix, transparent)
  if (g$race)                                       graph_race(var, df, ylims, save_image, upload, g$file_prefix, transparent)
  if (df_type(df) == "ky")                          graph_frl(var, df, ylims, save_image, upload, g$file_prefix, transparent)
  if (g$map != "")                                  graph_map(var, map_dfs, save_image = save_image, upload = upload, file_prefix = g$file_prefix)

  if (return_ylims) return(ylims)

  df_processed
}

#' Get y limits using multiple graph types
#'
#'@export
get_y_lims <- function (df, var){

  g <- graph_specs %>% filter(variable == var)

  g %>%
    as.list() %>%
    list2env(envir = parent.env(environment()))

  # Trendline
  df_single <- df %>% trend_data(var, rm, xmin, xmax)
  df_processed <- df_single

  # Max-min
  if(df_type(df) %in% c("FIPS", "MSA")){
    df_maxmin <- df %>%
      trend_data_maxmin(var, rm, xmin, xmax, order) %>%
      filter(variable %in% c("best", "worst")) %>%
      mutate(category = "total")

    df_processed %<>% bind_rows(df_maxmin)
  }

  # Sex
  if(sex){
    df_sex <- df %>% trend_data(var, rm_sex, xmin, xmax, cat = "sex", pctiles = sex_pctiles)
    df_processed %<>% bind_rows(df_sex)
  }

  # Race
  if(race){
    df_race <- df %>% trend_data(var, rm_race, xmin, xmax, cat = "race", pctiles = race_pctiles)
    df_processed %<>% bind_rows(df_race)
  }

  # FRL
  if(df_type(df) == "ky"){
    df_frl <- df %>% trend_data(var, rm, xmin, xmax, cat = "frl_status", pctiles = F)
    df_processed %<>% bind_rows(df_frl)
  }

  # Then use the min and max to determine y axis limits for trendline graphs.
  midpoint <- (max(df_processed[[var]], na.rm = TRUE) +
                 min(df_processed[[var]], na.rm = TRUE))/2

  border_space <- abs(.1 * midpoint)

  ylims <- c(min(df_processed[[var]], na.rm = TRUE) - border_space,
             max(df_processed[[var]], na.rm = TRUE) + border_space)

  if (units == "Percent" & ylims[2] > 100) ylims[2] <- 100

  glptools:::make_list(ylims, df_processed)
}

#' Create a ranking graph image of a GLP variable
#'
#' @param var A variable name
#' @export
graph_ranking <- function(var, df = "", save_image = T, upload = F, file_prefix = "", transparent = T, g = NULL){

  if (is.null(g)) g <- graph_specs %>% filter(variable == var)
  if (typeof(df) == "character") df <- get(paste0(g$data_frame, "_", g$geography))

  file_path <- paste0("output_images/", file_prefix, "_ranking.png")
  file_name <- paste0(var, "_ranking.png")

  r <- ranking(df,
               g$variable,
               plot_title = paste0(g$title, ", ", g$xmax),
               order = g$order,
               y_title = g$units,
               caption_text = g$caption,
               subtitle_text = g$subtitle)

  if (transparent) {
    r <- r +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
        legend.key = element_rect(fill = "transparent",colour = NA))

  }

  png(file_path, 3000, 2400, res = 200, bg = "transparent")
  print(r)
  dev.off()

  #print(r)
  #if (save_image) dev.off()
  if (upload) upload_image(g$driver, var, file_name)
}

#' Create a trendline graph image of a GLP variable
#'
#' @param var A variable name
#' @param ylims y-limits
#' @return A data frame of all data points found in the graphs
#' @export
graph_trendline <- function(var, df = "", ylims = "", save_image = T, upload = F, file_prefix = "", transparent = T, ...) {

  g <- graph_specs %>% filter(variable == var)
  if (typeof(df) == "character") df <- get(paste0(g$data_frame, "_", g$geography))

  file_path <- paste0("output_images/", file_prefix, "_trendline.png")
  file_name <- paste0(var, "_trendline.png")

  r <- trend(df,
             g$variable,
             rollmean = g$rm,
             xmin = g$xmin,
             xmax = g$xmax,
             plot_title = g$title,
             y_title = g$units,
             caption_text = g$caption,
             subtitle_text = g$subtitle,
             ylimits = ylims,
             ...)

  if (transparent) {
    r <- r +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
        legend.key = element_rect(fill = "transparent",colour = NA))
  }

  png(file_path, 3000, 2400, res = 200, bg = "transparent")
  print(r)
  dev.off()

  if (g$arrow != "") {
    if (g$subtitle != "" | g$rm > 1) spacing <- "subtitle"
    else spacing <- ""

    add_arrow(file_path, g$arrow, spacing)
  }
  if (upload) upload_image(g$driver, var, file_name)
}

#' Create a peer max and min graph image of a GLP variable
#'
#' @param var A variable name
#' @param ylims y-limits
#' @return A data frame of all data points found in the graphs
#' @export
graph_maxmin <- function(var, df = "",  ylims = "", save_image = T, upload = F, file_prefix = "", transparent = T, g = NULL, ...){

  if (is.null(g)) g <- graph_specs %>% filter(variable == var)
  if (typeof(df) == "character") df <- get(paste0(g$data_frame, "_", g$geography))

  file_path <- paste0("output_images/", file_prefix, "_max_min.png")
  file_name <- paste0(var, "_max_min.png")

  r <- trend_maxmin(df,
                    g$variable,
                    rollmean = g$rm,
                    xmin = g$xmin,
                    xmax = g$xmax,
                    order = g$order,
                    plot_title = g$title,
                    y_title = g$units,
                    caption_text = g$caption,
                    subtitle_text = g$subtitle,
                    ylimits = ylims,
                    ...)

  if (transparent) {
    r <- r +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
        legend.key = element_rect(fill = "transparent",colour = NA))
  }

  png(file_path, 3000, 2400, res = 200, bg = "transparent")
  print(r)
  dev.off()

  if (g$subtitle != "" | g$rm > 1) spacing <- "subtitle"
  else spacing <- ""

  if (g$arrow != "") add_arrow(file_path, g$arrow, spacing)
  if (upload) upload_image(g$driver, var, file_name)
}

#' Create a trendline graph image of a GLP variable broken down by sex
#'
#' @param var A variable name
#' @param ylims y-limits
#' @return A data frame of all data points found in the graphs
#' @export
graph_sex <- function(var, df = "", ylims = "", save_image = T, upload = F, file_prefix = "", transparent = T, g = NULL, ...) {

  if (is.null(g)) g <- graph_specs %>% filter(variable == var)
  if (typeof(df) == "character") df <- get(paste0(g$data_frame, "_", g$geography))

  file_path <- paste0("output_images/", file_prefix, "_sex.png")
  file_name <- paste0(var, "_sex.png")

  r <- trend(df,
             g$variable,
             rollmean = g$rm_sex,
             xmin = g$xmin,
             xmax = g$xmax,
             cat = "sex",
             plot_title = g$title %p% " by Sex",
             y_title = g$units,
             caption_text = g$caption_sex,
             subtitle_text = g$subtitle,
             ylimits = ylims,
             pctiles = g$sex_pctiles,
             ...)

  if (transparent) {
    r <- r +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
        legend.key = element_rect(fill = "transparent",colour = NA))
  }

  png(file_path, 3000, 2400, res = 200, bg = "transparent")
  print(r)
  dev.off()

  if(g$arrow != "") {
    if (g$subtitle != "" | g$rm_sex > 1) spacing <- "subtitle"
    else spacing <- ""

    add_arrow(file_path, g$arrow, spacing)
  }
  if (upload) upload_image(g$driver, var, file_name)
}

#' Create a trendline graph image of a GLP variable broken down by race
#'
#' @param var A variable name
#' @param ylims y-limits
#' @return A data frame of all data points found in the graphs
#' @export
graph_race <- function(var, df = "", ylims = "", save_image = T, upload = F, file_prefix = "", transparent = T, g = NULL, ...) {

  if (is.null(g)) g <- graph_specs %>% filter(variable == var)
  if (typeof(df) == "character") df <- get(paste0(g$data_frame, "_", g$geography))

  file_path <- paste0("output_images/", file_prefix, "_race.png")
  file_name <- paste0(var, "_race.png")

  r <- trend(df,
             g$variable,
             rollmean = g$rm_race,
             xmin = g$xmin,
             xmax = g$xmax,
             cat = "race",
             plot_title = g$title %p% " by Race",
             y_title = g$units,
             caption_text = g$caption_race,
             subtitle_text = g$subtitle,
             ylimits = ylims,
             pctiles = g$race_pctiles,
             ...)

  if (transparent) {
    r <- r +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
        legend.key = element_rect(fill = "transparent",colour = NA))
  }

  png(file_path, 3000, 2400, res = 200, bg = "transparent")
  print(r)
  dev.off()

  if (g$arrow != "") {
    if (g$subtitle != "" | g$rm_race > 1) spacing <- "subtitle"
    else if (g$data_frame == "education_ky") spacing <- "kyrace"
    else spacing <- ""

    add_arrow(file_path, g$arrow, spacing)
  }

  if (upload) upload_image(g$driver, var, file_name)
}

#' Create a trendline graph image of a GLP variable broken down by free/reduced lunch status
#'
#' @param var A variable name
#' @param ylims y-limits
#' @return A data frame of all data points found in the graphs
#' @export
graph_frl <- function(var, df = "", ylims = "", save_image = T, upload = F, file_prefix = "", transparent = T, g = NULL, ...) {

  if (is.null(g)) g <- graph_specs %>% filter(variable == var)
  if (typeof(df) == "character") df <- get(paste0(g$data_frame, "_", g$geography))

  file_path <- paste0("output_images/", file_prefix, "_frl.png")
  file_name <- paste0(var, "_frl.png")

  r <- trend(df,
             g$variable,
             rollmean = g$rm_race,
             xmin = g$xmin,
             xmax = g$xmax,
             cat = "frl_status",
             plot_title = g$title %p% " by FRL Status",
             y_title = g$units,
             caption_text = g$caption_race,
             subtitle_text = "Students in households below 185% of the poverty line are eligible for free/reduced lunch",
             ylimits = ylims,
             pctiles = g$variable == "act_composite",
             ...)

  if (transparent) {
    r <- r +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
        legend.key = element_rect(fill = "transparent",colour = NA))
  }

  png(file_path, 3000, 2400, res = 200, bg = "transparent")
  print(r)
  dev.off()

  if(g$arrow != "") {
    if (g$subtitle != "" | g$rm > 1) spacing <- "subtitle"
    else spacing <- ""

    add_arrow(file_path, g$arrow, spacing)
  }
  if (upload) upload_image(g$driver, var, file_name)
}

#' Create a leaflet map of a GLP variable
#'
#' @param var A variable name
#' @param ylims y-limits
#' @return A data frame of all data points found in the graphs
#' @export
graph_map <- function(var, map_dfs = "", save_image = T, upload = F, file_prefix = "", g = NULL) {

  if (is.null(g)) g <- graph_specs %>% filter(variable == var)

  file_name <- paste0(var, "_map.html")

  if (map_dfs == "") {
    df_tract <- get(paste0(g$data_frame, "_tract"))
    df_nh    <- get(paste0(g$data_frame, "_nh"))
    df_muw   <- get(paste0(g$data_frame, "_muw"))

    if (g$map == "tract") map_dfs <- list(df_tract, df_nh, df_muw)
    else                  map_dfs <- list(df_nh, df_muw)
  }

  if ("var_type" %in% names(map_dfs[[1]])) map_dfs %<>% map(function(x) filter(x, var_type == "percent"))

  m <- make_map(map_dfs,
                g$variable,
                g$popup_text,
                g$legend_title,
                units = g$units)
  setwd(paste0("output_images/", g$driver, "/", g$variable))
  htmlwidgets::saveWidget(m, file_name)
  setwd("../../../..")
  if (upload) upload_image(g$driver, var, file_name)

}

#' Add an arrow to a graph
#'
#' @param image_path Location of the image to which the arrow will be appended
#' @param arrow_name Name of the arrow file
#' @param spacing Used to determine vertical spacing of arrow.
#' \code{""}, \code{"subtitle"}, or \code{"kyrace"},
#' @export
add_arrow <- function(image_path, arrow_name, spacing){

  # Get arrow file based on word and subtitle
  suffix <- switch(spacing,
                   "subtitle" = "_st",
                   "kyrace" = "_kyrace")

  arrow <- magick::image_read_pdf("arrows/" %p% arrow_name %p% suffix %p% ".pdf")

  # Make arrow background transparent
  arrow <- magick::image_transparent(arrow, "#ffffff")

  # Get image to append
  image <- magick::image_read(image_path)

  # Append arrow to image
  output_img <- magick::image_append(c(image, arrow))

  # write image
  magick::image_write(output_img, image_path)
}

#' Upload an image to the GLP Google Drive
#'
#' @param driver Name of the driver. One of education, jobs, health, or qop.
#' @param variable Name of the variable
#' @param graph_type Graph suffix for type.
#'
upload_image <- function(driver, variable, file_name) {

  local_image  <- paste0("output_images/", driver, "/", variable, "/", file_name)
  drive_folder <- paste0("Website/Images/",  driver, "/", variable)
  drive_image  <- paste0("Website/Images/",  driver, "/", variable, "/", file_name)

  # Get contents of Google Drive folders
  folder = switch(driver,
                  education = "1AIA-h-1vKQKZq0M0qyygB9FE5JTBsZay",
                  jobs      = "1TMlmspdlNqt2U_dFqRzn4aWgfUrdUuY-",
                  health    = "1lvFHyr1NIpAMYa-L5BADPU4mBhJJe2Nm",
                  qop       = "1NCQx3SuMQmrFHK_BrcZcIPk2FwF4wR8g")

  current_files <- googledrive::drive_ls(path = googledrive::as_id(folder))

  # Check if folder for variable exists
  if(variable %in% current_files$name) {

    # If found, check for current file
    folder_id    <- current_files$id[current_files$name == variable]
    folder_files <- googledrive::drive_ls(path = googledrive::as_id(folder_id))

    if(file_name %in% folder_files$name) {

      # If found, update image
      image_id <- folder_files$id[folder_files$name == file_name]
      response <- googledrive::drive_update(googledrive::as_id(image_id), local_image, verbose = F)
    } else {
      # Otherwise, upload image
      response <- googledrive::drive_upload(local_image, drive_image, verbose = F)
    }
  } else {
    # If image folder is not found, make folder and upload image
    googledrive::drive_mkdir(drive_folder)

    response <- googledrive::drive_upload(local_image, drive_image, verbose = F)
  }
}
