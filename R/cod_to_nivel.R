


#transformando os códigos por para os níveis segundo a legislação atual

#' Convert Nivel da Funcao to nivel_funcao
#'
#' This function converts the values in the 'Nivel da Funcao' column to
#' the corresponding 'nivel_funcao' values based on specified conditions.
#'
#' @param data A data frame containing the input data.
#' @param column_name The name of the column to be used for conversion.
#' @return A modified data frame with the new 'nivel_funcao' column.
#' @examples
#' FT_SOF <- FT_SOF |> mutate(nivel_funcao = convert_nivel_funcao(., "Nivel da Funcao"))
#' @importFrom dplyr mutate case_when
#' @importFrom stringr grepl
#' @export
convert_nivel_funcao <- function(data, column_name) {
  data %>%
    mutate(nivel_funcao = case_when(
      grepl("\\d{2}01$", {{column_name}}) ~ "Níveis 1 e 2",
      grepl("\\d{2}02$", {{column_name}}) ~ "Níveis 1 e 2",
      grepl("\\d{2}03$", {{column_name}}) ~ "Níveis 3 e 4",
      grepl("\\d{2}04$", {{column_name}}) ~ "Níveis 3 e 4",
      grepl("\\d{2}05$", {{column_name}}) ~ "Níveis 5 e 6",
      grepl("\\d{2}06$", {{column_name}}) ~ "Níveis 5 e 6",
      grepl("\\d{2}07$", {{column_name}}) ~ "Níveis 7 a 9",
      grepl("\\d{2}08$", {{column_name}}) ~ "Níveis 7 a 9",
      grepl("\\d{2}09$", {{column_name}}) ~ "Níveis 7 a 9",
      grepl("\\d{2}10$", {{column_name}}) ~ "Níveis 10 a 12",
      grepl("\\d{2}11$", {{column_name}}) ~ "Níveis 10 a 12",
      grepl("\\d{2}12$", {{column_name}}) ~ "Níveis 10 a 12",
      grepl("\\d{2}13$", {{column_name}}) ~ "Níveis 13 e 14",
      grepl("\\d{2}14$", {{column_name}}) ~ "Níveis 13 e 14",
      grepl("\\d{2}15$", {{column_name}}) ~ "Níveis 15 e 16",
      grepl("\\d{2}16$", {{column_name}}) ~ "Níveis 15 e 16",
      grepl("\\d{2}17$", {{column_name}}) ~ "Nível 17",
      grepl("\\d{2}18$", {{column_name}}) ~ "Nível 18",
      TRUE ~ "Sem Função"
    ))
}


#' Convert Nivel da Funcao to DAS classification
#'
#' This function converts the values in the 'Nivel da Funcao' column to
#' the corresponding DAS classification based on specified conditions.
#'
#' @param data A data frame containing the input data.
#' @param column_name The name of the column to be used for conversion.
#' @return A modified data frame with the new 'DAS' column.
#' @examples
#' FT_SOF <- FT_SOF |> mutate(DAS = convert_nivel_to_DAS(., "Nivel da Funcao"))
#' @importFrom dplyr mutate case_when
#' @importFrom stringr grepl
#' @export
convert_nivel_to_DAS <- function(data, column_name) {
  data %>%
    mutate(DAS = case_when(
      grepl("\\d{2}01$", {{column_name}}) ~ "Níveis 1 e 2",
      grepl("\\d{2}02$", {{column_name}}) ~ "Níveis 1 e 2",
      grepl("\\d{2}03$", {{column_name}}) ~ "Níveis 3 e 4",
      grepl("\\d{2}04$", {{column_name}}) ~ "Níveis 3 e 4",
      grepl("\\d{2}05$", {{column_name}}) ~ "DAS 1",
      grepl("\\d{2}06$", {{column_name}}) ~ "DAS 1",
      grepl("\\d{2}07$", {{column_name}}) ~ "DAS 2",
      grepl("\\d{2}08$", {{column_name}}) ~ "DAS 2",
      grepl("\\d{2}09$", {{column_name}}) ~ "DAS 2",
      grepl("\\d{2}10$", {{column_name}}) ~ "DAS 3",
      grepl("\\d{2}11$", {{column_name}}) ~ "DAS 3",
      grepl("\\d{2}12$", {{column_name}}) ~ "DAS 3",
      grepl("\\d{2}13$", {{column_name}}) ~ "DAS 4",
      grepl("\\d{2}14$", {{column_name}}) ~ "DAS 4",
      grepl("\\d{2}15$", {{column_name}}) ~ "DAS 5",
      grepl("\\d{2}16$", {{column_name}}) ~ "DAS 5",
      grepl("\\d{2}17$", {{column_name}}) ~ "DAS 6",
      grepl("\\d{2}18$", {{column_name}}) ~ "NE",
      TRUE ~ "Sem Função"
    ))
}

