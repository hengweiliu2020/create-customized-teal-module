
# tm_g_bar module
# modification from tm_g_pp_vitals
# author: Harry Liu

template_bars <- function(dataname = "ANL",
                            aval_var = "SEX",
                            font_size = 12L, 
                          ggplot2_args = teal.widgets::ggplot2_args()
                            ) {

  y <- list()
  y$plot <- list()
  
  parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
    teal.widgets::resolve_ggplot2_args(
      user_plot = ggplot2_args,
      module_plot = teal.widgets::ggplot2_args(
        
        theme = list(
          text = substitute(ggplot2::element_text(size = font), list(font = font_size)),
          axis.text.y = quote(ggplot2::element_blank()),
          axis.ticks.y = quote(ggplot2::element_blank()),
          plot.title = substitute(ggplot2::element_text(size = font), list(font = font_size)),
          legend.position = "top",
          panel.grid.minor = quote(ggplot2::element_line(
            linewidth = 0.5,
            linetype = "dotted",
            colour = "grey"
          )),
          panel.grid.major = quote(ggplot2::element_line(
            linewidth = 0.5,
            linetype = "dotted",
            colour = "grey"
          ))
        )
      )
    ),
    ggtheme = "minimal"
  )

  lab_plot <- add_expr(
    list(),
    substitute_names(
      names = list(
        dataname = as.name(dataname),
        aval_var = as.name(aval_var)
      ),
      
      expr = {
        agg <- dataname %>%
          dplyr::count(aval_var)
      }
    )
  )

  lab_plot <- add_expr(
    lab_plot,
    substitute(
      expr = {
        dataname[[aval_char]] <- factor(dataname[[aval_char]])
        full_vita <- levels(dataname[[aval_char]])
        provided_vita <- full_vita
        
        all_colors <- stats::setNames(nestcolor::color_palette(length(full_vita), "stream"), full_vita)
        vars_colors <- all_colors[provided_vita]
        names(vars_colors) <- provided_vita
        
        result_plot <- ggplot2::ggplot(data = agg) + 
          ggplot2::geom_bar(
            data = agg,
            mapping = ggplot2::aes(y = n, x=aval_var, group=aval_var, col=aval_var, fill=aval_var), 
            stat='identity'
          ) +
          ggplot2::scale_color_manual(
            values = vars_colors
          ) 
        print(result_plot)
       
      },
      env = list(
        dataname = as.name(dataname),
        aval_var = as.name(aval_var),
        aval_char = aval_var,
        font_size_var = font_size
      )
    )
  )
  y$plot <- bracket_expr(lab_plot)
  y
}

tm_g_bar <- function(label,
                           dataname = "ADLB",
                           parentname = "ADSL",
                           patient_col = "USUBJID",
                           aval_var = NULL,
                           font_size = c(12L, 12L, 25L),
                           plot_height = c(500L, 500L, 500L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL
                            ){
 
  args <- as.list(environment())
  data_extract_list <- list(
    aval_var = `if`(is.null(aval_var), NULL, cs_to_des_select(aval_var, dataname = dataname))
  )

  module(
    label = label,
    ui = ui_g_bar,
    ui_args = c(data_extract_list, args),
    server = srv_g_bar,
    server_args = c(
      data_extract_list,
      list(
        dataname = dataname,
        parentname = parentname,
        label = label,
        plot_height = plot_height,
        plot_width = plot_width
      )
    ),
    datanames = c(dataname, parentname)
  )
}

ui_g_bar <- function(id, ...) {
  ui_args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    ui_args$aval_var
  )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("lab_plot")),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(ui_args[c( "aval_var")]),
      
      teal.transform::data_extract_ui(
        id = ns("aval_var"),
        label = "Select AVAL variable:",
        data_extract_spec = ui_args$aval_var,
        is_single_dataset = is_single_dataset_value
      ),
      teal.widgets::panel_item(
        title = "Plot settings",
        collapsed = TRUE,
        teal.widgets::optionalSliderInputValMinMax(
          ns("font_size"), "Font Size", ui_args$font_size,
          ticks = FALSE, step = 1
        )
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), button_label = "Show R code")
    ),
    pre_output = ui_args$pre_output,
    post_output = ui_args$post_output
  )
}

srv_g_bar <- function(id,
                         data,
                         reporter,
                         filter_panel_api,
                         dataname,
                         parentname,
                         aval_var,
                         plot_height,
                         plot_width,
                         label
                         ) {
  
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")

  moduleServer(id, function(input, output, session) {
  
    # lab tab ----

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list( aval_var = aval_var),
      datasets = data,
      select_validation_rule = list(
        
        aval_var = shinyvalidate::sv_required(
          "Please select AVAL variable."
        )
      )
    )

    anl_inputs <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = "dplyr::left_join"
    )
    
    anl_q <- reactive({
      data() %>%
        teal.code::eval_code(as.expression(anl_inputs()$expr))
    })
    
    merged <- list(anl_input_r = anl_inputs, anl_q = anl_q)
    
    all_q <- reactive({
      my_calls <- template_bars(
        dataname = "ANL",
        aval_var = input[[extract_input("aval_var", dataname)]],
        font_size = input[["font_size"]]
      ) 
        
      teal.code::eval_code(merged$anl_q(), as.expression(my_calls))
     
    })

    plot_r <- reactive(all_q()[["result_plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "lab_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(all_q())),
      title = label
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Bar Plot",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(teal.code::get_code(all_q()))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
