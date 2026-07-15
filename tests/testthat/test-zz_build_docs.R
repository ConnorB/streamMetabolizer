test_that("zz_build_docs() writes literal Rd tables", {
  withr::local_dir(tempdir())

  zz_build_docs()
  doc_text <- paste(readLines("man-roxygen/metab_data.R"), collapse = "\n")

  expect_match(doc_text, "#' @rawRd", fixed = TRUE)
  expect_match(doc_text, "#' \\section{Formatting \\code{data}}{", fixed = TRUE)
  expect_match(doc_text, "\\eqn{°C}", fixed = TRUE)
  expect_match(doc_text, "\\eqn{mgO_2 L^{-1}}", fixed = TRUE)
  expect_match(
    doc_text,
    "#'     \\strong{Example}:\n#'     \\tabular{lrrrrrr}{",
    fixed = TRUE
  )
})
