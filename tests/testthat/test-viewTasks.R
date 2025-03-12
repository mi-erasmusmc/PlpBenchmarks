library(PLPBenchmarks)
tasks <- viewBenchmarkTasks()

test_that("view tasks does not accept wrong arguments ", {
  expect_error(viewBenchmarkTasks(tasks = "few"))
  expect_error(viewBenchmarkTasks(tasks = "al"))
  expect_error(viewBenchmarkTasks(tasks = all))
  expect_error(viewBenchmarkTasks(tasks = 0))
  expect_error(viewBenchmarkTasks(tasks = "1"))
  # expect_error(viewBenchmarkTasks(tasks = 115))

  })

test_that("view tasks does not accept wrong arguments ", {
  expect_tibble(viewBenchmarkTasks(20115), nrows = 0)
  expect_tibble(viewBenchmarkTasks(1), nrows = 1, all.missing = F)
  expect_tibble(viewBenchmarkTasks(1:2), nrows = 2, all.missing = F)
  expect_tibble(viewBenchmarkTasks(1:20115), nrows = nrow(tasks), all.missing = F)
  expect_data_frame(viewBenchmarkTasks(1))
  # expect_subset(x = c("targetId, outcomeId"), choices = names(viewBenchmarkTasks(1)))
  # expect_names(x = names(viewBenchmarkTasks(1)), must.include = c("targetId, outcomeId") )
  # expect_subset(choices = c("targetId, outcomeId"), x = names(viewBenchmarkTasks(1)))
})