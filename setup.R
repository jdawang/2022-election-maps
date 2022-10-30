library(tidyverse)
library(sf)
library(readxl)
library(splines)
library(gstat)
library(mapview)
library(glue)
mapviewOptions(fgb = TRUE)

read_ward_data <- function(path, sheet, .range) {
  read_xlsx(
    path,
    sheet=sheet,
    range=.range
  ) %>%
    rename(candidate = Subdivision) %>%
    filter(!(candidate %in% c("Councillor", "Mayor"))) %>%
    pivot_longer(
      cols=!candidate,
      names_to="poll",
      names_transform=as.numeric,
      values_to="votes"
    ) %>%
    group_by(poll) %>%
    mutate(total = sum(votes), pct = ifelse(total != 0, votes / total, 0)) %>%
    ungroup()
}

add_ward_and_polls <- function(.data, long_code, short_code) {
  mutate(
    .data,
    ward=as.numeric(str_sub({{long_code}}, 1, 2)),
    poll=as.numeric({{short_code}}))
}

poll_locations = read_sf(
  glue("{here::here()}/data/Cot geospatial6 - Voting Location.geojson")
) %>%
  rename_all(str_to_lower) %>%
  add_ward_and_polls(point_long_code, point_short_code) %>%
  select(ward, poll, point_name, voter_count)

subdivision_locations <- read_sf(
  glue("{here::here()}/data/Cot geospatial6 - Voting Subdivision.geojson")
) %>%
  rename_all(str_to_lower) %>%
  add_ward_and_polls(area_long_code, area_short_code) %>%
  select(ward, poll, voter_count)

ranges <- str_c(
  "A2:",
  c(
    "BD19",
    "BU8",
    "CI9",
    "BS9",
    "BN6",
    "BB7",
    "AV6",
    "BM8",
    "BC12",
    "CQ15",
    "BZ17",
    "BM7",
    "CM12",
    "BA8",
    "BD7",
    "BK14",
    "BJ9",
    "BH7",
    "BF10",
    "BJ11",
    "AY9",
    "AY9",
    "AM6",
    "AZ7",
    "AW6"
  )
)

mayor_ranges <- str_replace(ranges, "^\\d+|\\d+$", "34")

councillor_results <- map2(
  1:25,
  ranges,
  ~ read_ward_data(
    glue("{here::here()}/data/2022-results/2022_Toronto_Poll_By_Poll_Councillor.xlsx"),
    glue("Ward {.x}"),
    .y
  )
)

mayor_results <- map2(
  1:25,
  mayor_ranges,
  ~ read_ward_data(
    glue("{here::here()}/data/2022-results/2022_Toronto_Poll_By_Poll_Mayor.xlsx"),
    glue("Ward {.x}"),
    .y
  )
)

merge_results_with_geometry <- function(results, geometry, ward_number) {
  results %>%
    left_join(
      filter(geometry, ward == ward_number),
      by="poll"
    ) %>%
    st_as_sf()
}

councillor_polls <- map2(
  1:25,
  councillor_results,
  ~ merge_results_with_geometry(.y, poll_locations, .x)
)
councillor_subdivisions <- map2(
  1:25,
  councillor_results,
  ~ merge_results_with_geometry(.y, subdivision_locations, .x)
)
mayor_polls <- map2(
  1:25,
  mayor_results,
  ~ merge_results_with_geometry(.y, poll_locations, .x)
)
mayor_subdivisions <- map2(
  1:25,
  mayor_results,
  ~ merge_results_with_geometry(.y, subdivision_locations, .x)
)

popupTable2 <- function(...) {
  pt_args <- list(...)
  pop <- do.call(leafpop::popupTable, pt_args)
  return(as.character(pop))
}

add_margin <- function(.data, winner, runner_up) {
  winner_only <- .data %>%
    filter(candidate == winner) %>%
    inner_join(
      .data %>%
        st_drop_geometry() %>%
        filter(candidate==runner_up) %>%
        rename(runner_up_pct = pct, runner_up_votes = votes) %>%
        select(poll, runner_up_pct, runner_up_votes),
      by="poll"
    ) %>%
    mutate(
      vote_margin = votes - runner_up_votes,
      vote_margin_pct = pct - runner_up_pct,
      vote_margin_pct_round = round(vote_margin_pct, digits=2),
      pct_round = round(pct, digits=2)
    )
}

margin_mapview <- function(
    poll_data,
    subdivision_data,
    winner,
    runner_up,
    palette,
    breaks=NA
) {
  poll_margins <- add_margin(poll_data, winner, runner_up)
  subdivision_margins <- add_margin(subdivision_data, winner, runner_up)

  if (is.na(breaks)) {
    breaks <- c(
      max(min(round(min(poll_margins$vote_margin_pct_round), 1) - 0.1, -0.5), -1),
      -0.3,
      -0.1,
      0.1,
      0.3,
      min(max(round(max(poll_margins$vote_margin_pct_round)) + 0.1, 0.5), 1)
    )
  }

  winner_split <- str_split(winner, fixed(" "))[[1]]
  winner_lname <- winner_split[-length(winner_split)]
  mapview(
    subdivision_margins,
    zcol="vote_margin_pct",
    col.regions=palette,
    at=breaks,
    map.types=c("Stamen.TonerLite", "Stamen.Toner"),
    popup=popupTable2(
      subdivision_margins,
      zcol=c(
        "candidate",
        "poll",
        "votes",
        "vote_margin",
        "total",
        "voter_count",
        "pct_round",
        "vote_margin_pct_round"
      ),
      feature.id=FALSE
    ),
    layer.name=glue("{winner_lname} margin - area"),
    label="vote_margin_pct_round"
  ) +
    mapview(
      poll_margins,
      zcol="vote_margin_pct",
      cex="total",
      col.regions=palette,
      at=breaks,
      map.types=c("Stamen.TonerLite", "Stamen.Toner"),
      popup=popupTable2(
        poll_margins,
        zcol=c(
          "candidate",
          "poll",
          "votes",
          "vote_margin",
          "total",
          "voter_count",
          "pct_round",
          "vote_margin_pct_round",
          "point_name"
        ),
        feature.id=FALSE
      ),
      layer.name=glue("{winner_lname} margin - poll"),
      label="vote_margin_pct_round",
      hide=TRUE
    )
}

popular_vote_mapview <- function(
    poll_data,
    subdivision_data,
    candidate_name,
    palette,
    breaks=NA
) {
  polls_candidate_only <- filter(poll_data, candidate == candidate_name) %>%
    mutate(pct_round=round(pct, digits=2))
  subdivision_candidate_only <- filter(subdivision_data, candidate == candidate_name) %>%
    mutate(pct_round=round(pct, digits=2))


  candidate_split <- str_split(candidate_name, fixed(" "))[[1]]
  candidate_lname <- candidate_split[-length(candidate_split)]
  candidate_lname <- str_flatten(candidate_lname, collapse=" ")

  if(is.na(breaks)) {
    breaks <- round(quantile(polls_candidate_only$pct, seq(0, 1, 0.2)), digits=2)
    breaks[1] <- max(breaks[1] - 0.1, 0)
    breaks[length(breaks)] <- min(breaks[length(breaks)] + 0.1, 1)
  }

  mapview(
    subdivision_candidate_only,
    zcol="pct",
    col.regions=palette,
    at=breaks,
    map.types=c("Stamen.TonerLite", "Stamen.Toner"),
    popup=popupTable2(
      subdivision_candidate_only,
      zcol=c(
        "candidate",
        "poll",
        "votes",
        "total",
        "voter_count",
        "pct_round"
      ),
      feature.id=FALSE
    ),
    layer.name=glue("{candidate_lname} vote - area"),
    label="pct_round",
    hide=TRUE
  ) +
    mapview(
      polls_candidate_only,
      zcol="pct",
      cex="total",
      col.regions=palette,
      at=breaks,
      map.types=c("Stamen.TonerLite", "Stamen.Toner"),
      popup=popupTable2(
        polls_candidate_only,
        zcol=c(
          "candidate",
          "poll",
          "votes",
          "total",
          "voter_count",
          "pct_round",
          "point_name"
        ),
        feature.id=FALSE
      ),
      layer.name=glue("{candidate_lname} vote - poll"),
      label="pct_round",
      hide=TRUE
    )
}
