test_that("excluding flagged trait values is reversible and preserves other traits", {
  shiny::testServer(mod_data_upload_server, {
    load_example(); session$flushReact()
    session$setInputs(col_gen = "Genotype", col_env = "Environment", col_rep = "Rep", col_block = "Block",
      col_row = "Row", col_col = "Col", col_traits = c("GrainYield", "PlantHeight"), design_manual = "RCBD")
    original <- raw_data()
    session$returned$exclude_values(c(1L, 3L), "GrainYield")
    session$flushReact()
    expect_true(all(is.na(raw_data()$GrainYield[c(1, 3)])))
    expect_equal(raw_data()$PlantHeight, original$PlantHeight)
    expect_equal(nrow(raw_data()), nrow(original))
    session$returned$restore_values(); session$flushReact()
    expect_equal(raw_data(), original)
  })
})
