library(testthat)

# test_that("Auth0 environment is created on package load", {
#   # Ensure the environment is NULL before loading the package
#   expect_null(._wt_auth_env_)
#
#   # Load the package to trigger the .onLoad function
#   wildrtrax:::.onLoad()
#
#   # Check that the environment is now initialized
#   expect_s3_class(._wt_auth_env_, "environment")
#   expect_equal(parent.env(._wt_auth_env_), .GlobalEnv)
# })
#
# test_that("Auth0 environment remains the same after multiple loads", {
#   initial_env <- ._wt_auth_env_
#
#   # Load the package again
#   wildrtrax:::.onLoad()
#
#   # Ensure the environment reference remains the same
#   expect_identical(._wt_auth_env_, initial_env)
# })
