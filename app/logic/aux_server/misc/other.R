box::use(
  sh = shiny,
  rt = shiny.router,
  rl = rlang,
  waiter,
  shj = shinyjs,
  future[...],
  R6[R6Class],
)

#' @export
#' Return to home page when return button is clicked
obs_return <- function(input) {
  sh$observeEvent(input$return, {
    rt$change_page("/")
  })
}

#' @export
#' Standardized html element for waiting screens
#' Provide html tags to customize content further
waiting_screen <- function(...) {
  dots <- rl$list2(...)
  sh$div(
    class = "fs-4 d-flex flex-column gap-3",
    waiter$spin_folding_cube(),
    !!!dots
  )
}

tips <- c(
  "Tips! Skrolla ner för att se en sammanfattning och tabeller",
  "Tips! Klicka på infoknappen i vyns övre vänstra hörn så kan du läsa mer om funktionerna",
  "Tips! Hovra över kartans reglage för att se motsvarande data på kartan",
  "Tips! Hovra över staplarna för att se exakta värden",
  "Tips! Klicka på rutorna nedanför stapeldiagram för att dölja eller visa en kategori",
  "Tips! Visste du att du kan expandera alla vyer med hjälp av symbolen i vyns nedre högra hörn?"
)

#' @export
#' Wrap a server into an observer which waits for an action button click on the home page
observe_home_waiter <- function(id, server, input) {
  sh$observeEvent(input[[paste0("home-", id)]], once = TRUE, {
    waiter$waiter_show(
      html = waiting_screen(sh$h1("Visualiseringar förbereds..."), sh$p(sample(tips, 1))),
      color = "#4161ab"
    )
    server
    shj$delay(5000, waiter$waiter_hide())
  })
}

#' @export
Worker <- R6Class("Worker", # nolint
    public = list(
        initialize = function() {
            plan(multicore)
        },
        run_job = function(id, fun, args_reactive, value_until_resolved = NULL,
                           invalidate_time = 1000) {
            sh$reactive({
                args_prepared <- args_reactive()

                result <- value_until_resolved

                if (!private$job_is_active(id)) {
                    private$job_schedule(id, fun, args_prepared)
                } else if (private$job_is_resolved(id)) {
                    result <- private$job_value(id)
                    private$job_reset(id)
                }

                if (!private$job_is_resolved(id)) sh$invalidateLater(invalidate_time)

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
