# lookup_usgs_elevation rejects a missing elevation

    Code
      lookup_usgs_elevation(39, -96, max_tries = 1)
    Condition
      Error in `lookup_usgs_elevation()`:
      ! The USGS Elevation Point Query Service returned no elevation for latitude 39 and longitude -96.
      i The service only covers the United States and its territories.

# lookup_usgs_elevation validates coordinates and units

    Code
      lookup_usgs_elevation(91, -96)
    Condition
      Error in `lookup_usgs_elevation()`:
      ! `latitude` must be a single finite number between -90 and 90.

---

    Code
      lookup_usgs_elevation(39, Inf)
    Condition
      Error in `lookup_usgs_elevation()`:
      ! `longitude` must be a single finite number between -180 and 180.

---

    Code
      lookup_usgs_elevation(39, -96, units = "yards")
    Condition
      Error in `lookup_usgs_elevation()`:
      ! `units` must be one of "m", "meters", "ft", or "feet" (case-insensitive), not "yards".

---

    Code
      lookup_usgs_elevation(39, -96, units = NA)
    Condition
      Error in `lookup_usgs_elevation()`:
      ! `units` must be a single string.

# lookup_usgs_elevation wraps request failures

    Code
      lookup_usgs_elevation(39, -96, max_tries = 1)
    Condition
      Error in `lookup_usgs_elevation()`:
      ! Failed to query the USGS Elevation Point Query Service.
      Caused by error in `httr2::req_perform()`:
      ! HTTP 503 Service Unavailable.
