interactive_surfaceplot <- function(data,
                                    amplitude,
                                    mesh,
                                    coords = NULL,
                                    template = NULL,
                                    col_range = NULL,
                                    col_scale = NULL,
                                    show_sensors = TRUE
){
  stop_if_missing_cols(data, required_cols = c(amplitude, "sensor"))

  if (any(is.na(data[[amplitude]]))) {
    stop("There are NA's in amplitude column.")
  }

  if (!(is.logical(show_sensors))) {
    stop("Argument 'show_sensors' has to be logical.")
  }

  if (!is.null(template) && !is.null(coords)) {
    warning("Both 'template' and 'coords' were specified. Using 'template' and ignoring 'coords'.")
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    template <- "HCGSN256"
  }

  if (inherits(data, "tbl_sql") || inherits(data, "tbl_dbi")) {
    data <- dplyr::collect(data) # collect data for DB table
  }

  sensor_select <- unique(data$sensor)

  if (!is.null(template)) {
    coords_full <- switch(template,
                          "HCGSN256" = diegr::HCGSN256$D2,
                          stop("Unknown template.")
    )
    sensor_index <- which(coords_full$sensor %in% sensor_select)
    coords <- coords_full[sensor_index,]
  }

  stop_if_missing_cols(coords, required_cols = c("x", "y", "sensor"))

  if (missing(mesh)) {
    mesh <- point_mesh(dimension = 2, template = "HCGSN256",
                       sensor_select = sensor_select)
  }

  if (control_D2(mesh)) {
    mesh_mat <- mesh$D2
    tri <- make_triangulation(mesh_mat)
  }

  coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]])

  if (!all(unique(coords$sensor) %in% data$sensor)) {
    stop("Mismatch between sensors in data and coords.")
  }

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
    mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
    arrange(.data$sensor)

  y_hat <- IM(coords_df, data_order[[amplitude]], mesh_mat)$Y_hat
  ycp_IM <- y_hat[1:dim(mesh_mat)[1]]
  interp_data <- data.frame(x = mesh_mat[,1], y = mesh_mat[,2], ycp_IM = ycp_IM)

  if (is.null(col_scale)) {
    if (is.null(col_range)) {
      col_range <- range(interp_data$ycp_IM)
    }

    col_scale <- create_scale(col_range)
  }

  col_scale_plotly <- make_plotly_scale(col_scale)


  fig <- plot_ly() |>
    add_trace(
      type = "mesh3d",
      opacity = 0.85,

      x = interp_data$x,
      y = interp_data$y,
      z = interp_data$ycp_IM,

      i = tri[,1] - 1,
      j = tri[,2] - 1,
      k = tri[,3] - 1,

      intensity = interp_data$ycp_IM,
      colorscale = col_scale_plotly,
      cmin = min(col_scale$breaks),
      cmax = max(col_scale$breaks)
    )

  if (show_sensors == TRUE) {
    fig <- fig |>
      add_trace(
        type = "scatter3d",
        mode = "markers",

        x = coords$x,
        y = coords$y,
        z = data_order[[amplitude]],

        marker = list(
          size = 2,
          color = "black"
        )
      )
  }

  return(fig)
}



make_plotly_scale <- function(col_scale) {

  vals <- (col_scale$breaks - min(col_scale$breaks)) / diff(range(col_scale$breaks))

  out <- list()

  for (i in seq_along(col_scale$colors)) {

    out[[2*i - 1]] <- list(vals[i],   col_scale$colors[i])
    out[[2*i    ]] <- list(vals[i + 1], col_scale$colors[i])
  }

  out
}



interactive_surfaceplot_curves <- function(data,
                                           sensor_ticks = NULL,
                                           col_range = NULL,
                                           col_scale = NULL
){

  if (inherits(data, "tbl_sql") || inherits(data, "tbl_dbi")) {
    data <- dplyr::collect(data) # collect data for DB table
  }

  if (!all(unique(sensor_ticks) %in% data$sensor)) {
    stop("Mismatch between sensors in data and sensor ticks.")
  }

  if (is.null(col_scale)) {
    if (is.null(col_range)) {
      col_range <- range(data$average)
    }

    col_scale <- create_scale(col_range)
  }

  col_scale_plotly <- make_plotly_scale(col_scale)

  # create data structure for plotly
  data$sensor <- factor(data$sensor, levels = unique(data$sensor))
  mat <- stats::xtabs(average ~ sensor + time, data = data)
  times <- as.numeric(colnames(mat))
  sensors <- rownames(mat)

  fig <- plot_ly(z = ~as.matrix(mat))
  fig <- fig |>
    add_surface(
      colorscale = col_scale_plotly,
      cmin = min(col_scale$breaks),
      cmax = max(col_scale$breaks),
      colorbar = list(title = "Amplitude (\u00b5V)")
    )

  selected_tickvals <- which(sensors %in% sensor_ticks)
  selected_ticktext <- sensors[selected_tickvals]

  fig <- fig %>% layout(
    scene = list(
      yaxis = list(
        title = "Sensors",
        tickmode = "array",
        tickvals = selected_tickvals,
        ticktext = selected_ticktext
      ),
      xaxis = list(
        title = "Time (ms)",
        autorange = TRUE
      ),
      zaxis = list(
        title = "Amplitude (\u00b5V)"
      )
    )
  )

  return(fig)
}
