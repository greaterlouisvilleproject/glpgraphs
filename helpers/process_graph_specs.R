library(dplyr)
library(tidyr)
library(magrittr)
library(glptools)
library(glpdata)

process_graph_specs <- function(df, driver){
  all_cols <- c("variable", "data_frame", "geography", "title", "subtitle",  "units", "xmin", "xmax", "arrow", "improvement",
                "sex", "race", "sex_pctiles", "race_pctiles", "rolling_mean", "rm_race", "rm_sex",
                "map", "legend_title", "popup_text", "source", "source_sex", "source_race")

  missing_cols <- df %cols_not_in% all_cols
  df[missing_cols] <- NA
  df <- df[all_cols]

  df %<>%
    transmute(
      variable,
      data_frame,
      geography,
      title,
      arrow = replace_na(arrow, ""),
      order = if_else(is.na(improvement) | improvement == "higher", "descending", "ascending"),

      units = replace_na(units, "Percent"),
      units = replace(units, units == "none", ""),

      subtitle = replace_na(subtitle, ""),
      legend_title,
      popup_text = if_else(is.na(popup_text), title, popup_text),

      xmin,
      xmax,

      sex,
      race,
      map  = replace_na(map, ""),

      rm = replace_na(rolling_mean, 1),
      rm = pmax(rm, rm_race, rm_sex, na.rm = TRUE),

      rm_race = pmax(rm, rm_race, rm_sex, na.rm = TRUE),
      rm_sex  = pmax(rm, rm_race, rm_sex, na.rm = TRUE),

      caption      = paste0("Source: Greater Louisville Project
                                 Data from ", source),
      caption_race = if_else(
        is.na(source_race), caption,
        paste0("Source: Greater Louisville Project
                                   Data from ", source_race)),
      caption_sex  = if_else(
        is.na(source_sex), caption,
        paste0("Source: Greater Louisville Project
                                   Data from ", source_sex)),

      sex_pctiles,
      race_pctiles,

      driver = d,

      folder = paste0(d, "/", variable),

      file_prefix = paste0(folder, "/", variable))

  # replace NA xmins and xmaxes

  for(r in 1:nrow(df)){
    tryCatch({
      if(is.na(df[[r, "xmin"]])) {
        df[[r, "xmin"]] <-
          min(years_in_df(get(paste0(df[[r, "data_frame"]], "_", df[[r, "geography"]])), df[[r, "variable"]]))
      }
      if(is.na(df[[r, "xmax"]])) {
        df[[r, "xmax"]] <-
          max(years_in_df(get(paste0(df[[r, "data_frame"]], "_", df[[r, "geography"]])), df[[r, "variable"]]))
      }
    },
    error = function(cond){})
  }


  df
}

for(d in c("education", "jobs", "health", "qop")){
  driver_graph_specs <- readxl::read_xlsx("helpers/graphs.xlsx", sheet = d,
                                   col_types = c("text", "text", "text", "text", "text", "text",
                                                 "numeric", "numeric", "text", "text",
                                                 "logical", "logical", "logical", "logical",
                                                 "numeric", "numeric", "numeric",
                                                 "text", "text", "text",
                                                 "text", "text", "text"))

  driver_graph_specs %<>% process_graph_specs(d)

  output <- assign_row_join(output, driver_graph_specs)
}

graph_specs <- output

rm(output, d, process_graph_specs, driver_graph_specs)
