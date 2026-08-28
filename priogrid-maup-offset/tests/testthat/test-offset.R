test_that("pg_offset_grid shifts east without touching latitude", {
  cfg <- pg_config(extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10))
  offset_cfg <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 90, reference_lat = 0)

  expect_s3_class(offset_cfg, "pg_config")
  expect_equal(unname(offset_cfg$extent["ymin"]), unname(cfg$extent["ymin"]))
  expect_equal(unname(offset_cfg$extent["ymax"]), unname(cfg$extent["ymax"]))
  expect_gt(offset_cfg$extent["xmin"], cfg$extent["xmin"])
  expect_gt(offset_cfg$extent["xmax"], cfg$extent["xmax"])

  # At the equator, 1 degree of longitude is ~111.32 km.
  expected_shift <- 5 / 111.32
  expect_equal(unname(offset_cfg$extent["xmin"] - cfg$extent["xmin"]), expected_shift, tolerance = 1e-6)
  expect_equal(unname(offset_cfg$extent["xmax"] - cfg$extent["xmax"]), expected_shift, tolerance = 1e-6)
})

test_that("pg_offset_grid leaves nrow, ncol and crs unchanged", {
  cfg <- pg_config(nrow = 20L, ncol = 40L, extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10))
  offset_cfg <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 90)

  expect_equal(offset_cfg$nrow, cfg$nrow)
  expect_equal(offset_cfg$ncol, cfg$ncol)
  expect_equal(offset_cfg$crs, cfg$crs)
  expect_equal(offset_cfg$temporal_resolution, cfg$temporal_resolution)
  expect_equal(offset_cfg$start_date, cfg$start_date)
})

test_that("pg_offset_grid accounts for latitude when converting km to degrees", {
  cfg <- pg_config(extent = c(xmin = 0, xmax = 10, ymin = 55, ymax = 65))

  shift_at_equator <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 90, reference_lat = 0)
  shift_at_60      <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 90, reference_lat = 60)

  dx_equator <- unname(shift_at_equator$extent["xmin"] - cfg$extent["xmin"])
  dx_60      <- unname(shift_at_60$extent["xmin"] - cfg$extent["xmin"])

  # A fixed km offset covers more degrees of longitude further from the equator.
  expect_gt(dx_60, dx_equator)
})

test_that("pg_offset_grid defaults reference_lat to the extent's vertical midpoint", {
  cfg <- pg_config(extent = c(xmin = 0, xmax = 10, ymin = 40, ymax = 60))
  offset_default <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 90)
  offset_explicit <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 90, reference_lat = 50)

  expect_equal(offset_default$extent, offset_explicit$extent)
})

test_that("pg_offset_grid supports arbitrary bearings", {
  cfg <- pg_config(extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10))

  north <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 0, reference_lat = 0)
  expect_equal(unname(north$extent["xmin"]), unname(cfg$extent["xmin"]), tolerance = 1e-10)
  expect_gt(north$extent["ymin"], cfg$extent["ymin"])

  south <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 180, reference_lat = 0)
  expect_lt(south$extent["ymin"], cfg$extent["ymin"])

  west <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 270, reference_lat = 0)
  expect_lt(west$extent["xmin"], cfg$extent["xmin"])
})

test_that("pg_offset_grid treats non-4326 CRS extents as metres", {
  cfg <- pg_config(crs = "epsg:3857", extent = c(xmin = 0, xmax = 100000, ymin = 0, ymax = 100000))
  offset_cfg <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 90)

  expect_equal(unname(offset_cfg$extent["xmin"] - cfg$extent["xmin"]), 5000)
  expect_equal(unname(offset_cfg$extent["xmax"] - cfg$extent["xmax"]), 5000)
  expect_equal(unname(offset_cfg$extent["ymin"]), unname(cfg$extent["ymin"]))
})

test_that("pg_offset_grid validates its arguments", {
  cfg <- pg_config()
  expect_error(pg_offset_grid("not a config"), "pg_config object")
  expect_error(pg_offset_grid(cfg, distance_km = "5"), "distance_km")
  expect_error(pg_offset_grid(cfg, bearing_deg = c(1, 2)), "bearing_deg")
  expect_error(pg_offset_grid(cfg, reference_lat = 95), "reference_lat")
})

test_that("pg_offset_grid produces a raster congruent to the original, only shifted", {
  skip_if_not_installed("terra")

  cfg <- pg_config(nrow = 10L, ncol = 10L, extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10))
  offset_cfg <- pg_offset_grid(cfg, distance_km = 5, bearing_deg = 90, reference_lat = 0)

  pg <- prio_blank_grid(cfg)
  pg_offset <- prio_blank_grid(offset_cfg)

  expect_equal(terra::res(pg), terra::res(pg_offset))
  expect_equal(terra::ncol(pg), terra::ncol(pg_offset))
  expect_equal(terra::nrow(pg), terra::nrow(pg_offset))
  expect_false(isTRUE(all.equal(terra::ext(pg), terra::ext(pg_offset))))
})
