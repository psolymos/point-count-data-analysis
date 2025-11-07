library(shiny)
library(unmarked)
library(reactable)
library(ggplot2)

SEED <- 0
TITLE <- "Multiple-visits"
L_fun_occu <- function(Y, p, phi, ydot, T) {
    L <- prod(
        phi *
            (choose(T, ydot) * p^ydot * (1 - p)^(T - ydot)) +
            (1 - phi) * (ydot == 0)
    )
    L
}
L_fun_pcount <- function(Y, p, lambda, n, T, Y_max, K = 50) {
    nll <- unmarked:::nll_pcount(
        beta = c(log(lambda), qlogis(p)),
        n_param = c(1, 1, 0),
        y = Y,
        X = matrix(1, n, 1),
        V = matrix(1, n * T, 1),
        X_offset = rep(0, n),
        V_offset = matrix(1, n * T, 1),
        K = K,
        Kmin = Y_max,
        mixture = 1,
        threads = 1
    )
    exp(-nll)
}

ui <- navbarPage(
    TITLE,
    tabPanel(
        "Occupancy",
        column(
            4,
            sliderInput(
                "phi_true",
                "True occupancy probability (phi)",
                0,
                1,
                0.4,
                0.01
            ),
            sliderInput(
                "p_true",
                "True detection probability (p)",
                0,
                1,
                0.6,
                0.01
            ),
            sliderInput(
                "n_sites",
                "Number of sites (n)",
                2,
                100,
                50,
                1
            ),
            sliderInput(
                "n_visits",
                "Number of visits to the sites (T)",
                1,
                20,
                5,
                1
            ),
            actionButton("seed", "Change random seed")
        ),
        column(8, plotOutput("surface"), reactableOutput("estimates"))
    ),
    tabPanel(
        "Abundance",
        column(
            4,
            sliderInput(
                "lam_true",
                "True abundance (lambda)",
                0,
                20,
                4,
                0.1
            ),
            sliderInput(
                "p_true2",
                "True detection probability (p)",
                0,
                1,
                0.6,
                0.01
            ),
            sliderInput(
                "n_sites2",
                "Number of sites (n)",
                2,
                100,
                50,
                1
            ),
            sliderInput(
                "n_visits2",
                "Number of visits to the sites (T)",
                1,
                10,
                5,
                1
            ),
            actionButton("seed2", "Change random seed")
        ),
        column(8, plotOutput("surface2"), reactableOutput("estimates2"))
    )
)

server <- function(input, output, session) {
    rv <- reactiveValues(seed = SEED, seed2 = SEED)

    observeEvent(input$seed, {
        rv$seed <- rv$seed + 1
        showNotification(sprintf("Random seed changed to %s.", rv$seed))
    })
    W <- reactive({
        set.seed(rv$seed)
        rbinom(n = input$n_sites, size = 1, prob = input$phi_true)
    })
    Y <- reactive({
        set.seed(rv$seed)
        Y <- replicate(
            input$n_visits,
            rbinom(n = input$n_sites, size = W(), prob = input$p_true)
        )
        if (input$n_visits < 2) {
            Y <- data.matrix(Y)
        }
        Y
    })
    Y_max <- reactive({
        apply(Y(), 1, max)
    })
    est_naive <- reactive({
        c(phi = mean(Y_max()), p = mean(Y()) / mean(Y_max()))
    })
    est_mvocc <- reactive({
        mod <- unmarked::occu(
            formula = ~1 ~ 1,
            data = unmarked::unmarkedFrameOccu(y = Y())
        )
        c(
            phi = plogis(coef(mod, type = "state")),
            p = plogis(coef(mod, type = "det"))
        )
    })
    g <- 100
    grid <- expand.grid(
        p = seq(0, 1, length.out = g),
        phi = seq(0, 1, length.out = g)
    )
    L <- reactive({
        L <- numeric(nrow(grid))
        for (i in seq_along(L)) {
            L[i] <- L_fun_occu(
                Y = Y(),
                p = grid$p[i],
                phi = grid$phi[i],
                ydot = rowSums(Y()),
                T = input$n_visits
            )
        }
        L / max(L)
    })
    output$surface <- renderPlot({
        gd <- data.frame(grid, L = L())
        print(gd[which.max(gd$L), ])
        ggplot(data = gd, mapping = aes(x = p, y = phi, z = L)) +
            geom_contour_filled(show.legend = FALSE) +
            geom_hline(yintercept = input$phi_true, col = 2) +
            geom_vline(xintercept = input$p_true, col = 2) +
            theme_minimal()
    })
    output$estimates <- renderReactable({
        d <- data.frame(
            Parameter = c("phi", "p"),
            Truth = c(input$phi_true, input$p_true),
            Naive = round(est_naive(), 3),
            "Bias, Naive" = round(
                est_naive() - c(input$phi_true, input$p_true),
                3
            ),
            "MV Occ" = round(est_mvocc(), 3),
            "Bias, MC Occ" = round(
                est_mvocc() - c(input$phi_true, input$p_true),
                3
            ),
            check.names = FALSE
        )
        reactable(d, rownames = FALSE)
    })

    observeEvent(input$seed2, {
        rv$seed2 <- rv$seed2 + 1
        showNotification(sprintf("Random seed changed to %s.", rv$seed2))
    })
    N <- reactive({
        set.seed(rv$seed2)
        rpois(n = input$n_sites2, lambda = input$lam_true)
    })
    Y2 <- reactive({
        set.seed(rv$seed2)
        Y <- replicate(
            input$n_visits2,
            rbinom(n = input$n_sites2, size = N(), prob = input$p_true2)
        )
        if (input$n_visits2 < 2) {
            Y <- data.matrix(Y)
        }
        Y
    })
    Y_max2 <- reactive({
        apply(Y2(), 1, max)
    })
    est_naive2 <- reactive({
        c(lambda = mean(Y_max2()), p = mean(Y2()) / mean(Y_max2()))
    })
    est_mvabu <- reactive({
        mod <- suppressWarnings(unmarked::pcount(
            formula = ~1 ~ 1,
            data = unmarked::unmarkedFramePCount(y = Y2())
        ))
        c(
            lambda = exp(coef(mod, type = "state")),
            p = plogis(coef(mod, type = "det"))
        )
    })
    g2 <- 50
    grid2 <- expand.grid(
        p = seq(0, 1, length.out = g2),
        lambda = seq(0, 2, length.out = g2)
    )
    L2 <- reactive({
        L <- numeric(nrow(grid2))
        gr <- grid2
        gr$lambda <- gr$lambda * input$lam_true
        for (i in seq_along(L)) {
            L[i] <- L_fun_pcount(
                Y = Y2(),
                p = gr$p[i],
                lambda = gr$lambda[i],
                n = input$n_sites2,
                T = input$n_visits2,
                Y_max = Y_max2()
            )
        }
        L / max(L)
    })
    output$surface2 <- renderPlot({
        gd <- grid2
        gd$lambda <- gd$lambda * input$lam_true
        gd$L <- L2()
        print(gd[which.max(gd$L), ])
        ggplot(data = gd, mapping = aes(x = p, y = lambda, z = L)) +
            geom_contour_filled(show.legend = FALSE) +
            geom_hline(yintercept = input$lam_true, col = 2) +
            geom_vline(xintercept = input$p_true2, col = 2) +
            theme_minimal()
    })
    output$estimates2 <- renderReactable({
        d <- data.frame(
            Parameter = c("lambda", "p"),
            Truth = c(input$lam_true, input$p_true2),
            Naive = round(est_naive2(), 3),
            "Bias, Naive" = round(
                est_naive2() - c(input$lam_true, input$p_true2),
                3
            ),
            "MV Abu" = round(est_mvabu(), 3),
            "Bias, MC Abu" = round(
                est_mvabu() - c(input$lam_true, input$p_true2),
                3
            ),
            check.names = FALSE
        )
        reactable(d, rownames = FALSE)
    })
}

shinyApp(ui = ui, server = server)
