control <- ergm::control.ergm(
  seed = 2022)
alpha <- 0.25

add_subnames <- function(df){
  var_label(df) <- df  |> 
    head(1) |> 
    mutate_all(as.character) |> 
    mutate_all(\(df) df |> str_replace_all(pattern = "\\n", replacement = " "))
  
  df |> slice(-1) |> as_tibble()
}

graph_to_ergm <- function(df,
                          connection = Q13,
                          weight_cut = 4,
                          net = T,
                          type = "remove_vero"){
  ##############################################################
  ## wraps filtering of edges and nodes
  ##############################################################
  
  if(type == "remove_vero"){
    df |>
      filter(!alter_id |> str_detect("vero")) |>
      select(id, alter_id, {{connection}}, remove_team) |>
      graph_from_data_frame(directed = T) |>
      as_tbl_graph() |>
      activate(edges) |>
      mutate(weight = {{connection}} |> as.integer()) |>
      filter(!is.na(weight)) |>
      filter(weight >= weight_cut) |>
      filter(remove_team != 1) |>
      activate(nodes) |>
      left_join(
        df_ego_chars,
        by = c("name" = "net_id")
      ) |>
      left_join(
        df_teams_clean |>
          select(id, Team_id, self_leader) |>
          unique(), by = c("name" = "id")
      ) |>
      left_join(
        meta_ego,
        by = c("name" = "id")
      ) |>
      filter(!is.na(Team_id),
             !is.na(age_ego),
             !is.na(nationality_ego),
             !is.na(race_ego),
             !is.na(density_egonet)) |>
      mutate(condition = Team_id |>
               stringi::stri_sub(1, 1)) |>
      activate(nodes) |>
      group_by(Team_id) |>
      mutate(n = n()) |>
      filter(n > 1) |> # remove teams with only one member
      ungroup() |>
      activate(edges)  -> g
    
    
    if (net) {
      return(g |> intergraph::asNetwork(loops = FALSE))
    } else {
      return(g)
    }
  } else if (type == "vero"){
    df |>
      select(id, alter_id, {{connection}}, remove_team) |>
      mutate(to_vero = alter_id |> str_detect("vero")) |>
      graph_from_data_frame(directed = T) |>
      as_tbl_graph() |>
      activate(edges) |>
      filter(remove_team == 0) |>
      mutate(weight = {{connection}} |> as.integer()) |>
      filter(!is.na(weight)) |> # remove NAs
      filter(weight >= 4) |>  # remove low weights
      filter(to_vero == TRUE) |> # remove non-vero
      activate(nodes) |>
      left_join(
        df_teams_clean |>
          select(id, Team_id) |>
          unique(),
        by = c("name" = "id"))  |>
      mutate(condition = Team_id |>
               stringi::stri_sub(1, 1)) |>
      activate(nodes) |>
      group_by(Team_id) |>
      mutate(n = n()) |>
      filter(n > 1) |> # remove teams with only one member
      ungroup() |>
      activate(edges) -> g
    
    if (net) {
      return(g |> intergraph::asNetwork(loops = FALSE))
    } else {
      return(g)
    }
  } else if (type == "human"){
    df |>
      select(id, alter_id, {{connection}}, remove_team) |>
      mutate(to_vero = alter_id |> str_detect("vero")) |>
      graph_from_data_frame(directed = T) |>
      as_tbl_graph() |>
      activate(edges) |>
      mutate(weight = {{connection}} |> as.integer()) |>
      filter(!is.na(weight)) |>
      filter(weight >= weight_cut) |>
      filter(remove_team != 1) |>
      # filter(!to_vero) |> ## remove vero
      activate(nodes) |>
      left_join(
        df_ego_chars,
        by = c("name" = "net_id")
      ) |>
      left_join(
        df_teams_clean |>
          select(id, Team_id, self_leader) |>
          unique(), by = c("name" = "id")
      ) |>
      left_join(
        meta_ego,
        by = c("name" = "id")
      ) |>
      filter(!is.na(Team_id),
             !is.na(age_ego),
             !is.na(nationality_ego),
             !is.na(race_ego),
             !is.na(density_egonet)) |>
      mutate(condition = Team_id |>
               stringi::stri_sub(1, 1)) |>
      activate(nodes) |>
      group_by(Team_id) |>
      mutate(n = n()) |>
      filter(n > 1) |> # remove teams with only one member
      ungroup() |>
      activate(edges)  -> g
    
    
    if (net) {
      return(g |> intergraph::asNetwork(loops = FALSE))
    } else {
      return(g)
    }
  }
}

# graph_to_ergm_v <- function(df,
#                             connection = Q82,
#                             weight_cut = 4,
#                             net = T){
#   df |>
#     select(id, alter_id, {{connection}}, remove_team) |>
#     graph_from_data_frame(directed = T) |>
#     as_tbl_graph() |>
#     activate(edges) |>
#     filter(remove_team == 0) |>
#     filter(!is.na({{connection}})) |> # remove NAs
#     filter({{connection}} >= 4) |>  # remove low weights
#     filter(alter_id |> str_detect("vero")) |> # remove non-vero
#     activate(nodes) |>
#     left_join(
#       df_teams_clean |>
#         select(id, Team_id) |>
#         unique(),
#       by = c("name" = "id"))  |>
#     mutate(condition = Team_id |>
#       stringi::stri_sub(-1)) |>
#     activate(nodes) |>
#     group_by(Team_id) |>
#     mutate(n = n()) |>
#     filter(n > 1) |> # remove teams with only one member
#     ungroup() |>
#     activate(edges) -> g
#
#   if (net) {
#     return(g |> intergraph::asNetwork(loops = FALSE))
#   } else {
#     return(g)
#   }
# }