get_icms <- function() {
  tb_ckan <- ckanr::resource_show(id="48f9698b-fa39-4337-82ca-df5c0b02d866",url="https://dados.gov.br/")

  URL_add <- tb_ckan$url

  tmp <- tempfile(fileext = ".xls")
  download.file(URL_add,mode = "wb", destfile = tmp, extra = "-k", method = "curl")

  icms <- readxl::read_xls(tmp, sheet = 2, skip = 1)
  icms <- icms[,c(1,2,4,5,22)]

  icms %>% dplyr::transmute(
      uf    = factor(icms$id_uf, levels = unique(icms$id_uf), labels = unique(icms$ESTADO)),
      data  = as.Date(str_c(ANO, "-", MÊS, "-01")),
      valor = `TOTAL DA ARRECADAÇÃO DO ICMS`)
}
