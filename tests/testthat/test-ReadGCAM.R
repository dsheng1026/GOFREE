
test_path <- system.file('data', package = 'GCAMUSAJobs')

test_that("throw error message if the input path doesn't exist", {
  testthat::expect_error(ReadGCAM(input_path = test_path))
})

test_that("throw error message if the input path is not correctly specified", {
  testthat::expect_error(ReadGCAM(filetype = 'db',
                                  input_path = test_path,
                                  db_name = "database_basexdbGCAM-USA_Ref",
                                  scen_name = "GCAM-USA_Ref", # this needs to match the scenario name specified in the DB
                                  prj_name = "mydata.dat"))
})
