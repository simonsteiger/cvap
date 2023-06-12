box::use(
    future[...],
    shiny[reactive, invalidateLater],
    R6[R6Class],
)

#' @export
Worker <- R6Class("Worker", # nolint
    public = list(
        initialize = function() {
            plan(multicore)
        },
        run_job = function(id, fun, args_reactive, value_until_resolved = NULL,
                           invalidate_time = 1000) {
            reactive({
                args_prepared <- args_reactive()

                result <- value_until_resolved

                if (!private$job_is_active(id)) {
                    private$job_schedule(id, fun, args_prepared)
                } else if (private$job_is_resolved(id)) {
                    result <- private$job_value(id)
                    private$job_reset(id)
                }

                if (!private$job_is_resolved(id)) invalidateLater(invalidate_time)

                list(result = result, resolved = private$job_is_resolved(id))
            })
        },
        get_job_registry = function() {
            private$job_registry
        }
    ),
    private = list(
        job_registry = list(),
        job_schedule = function(id, fun, args) {
            private$job_registry[[id]] <- future(fun(args))
            if (isTRUE(getOption("worker.debug.mode"))) {
                print(paste("Job scheduled:", id))
            }
        },
        job_is_resolved = function(id) {
            result <- resolved(private$job_registry[[id]])
            if (isTRUE(getOption("worker.debug.mode"))) {
                print(paste("Resolved?", result))
            }
            result
        },
        job_is_active = function(id) {
            !is.null(private$job_registry[[id]])
        },
        job_value = function(id) {
            if (is.null(private$job_registry[[id]])) {
                NULL
            } else {
                value(private$job_registry[[id]])
            }
        },
        job_reset = function(id) {
            private$job_registry[[id]] <- NULL
        }
    )
)

#' @export
initialize_worker <- function() {
    Worker$new()
}
