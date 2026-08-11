#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' apPayableUploadServer()
apPayableUploadServer <- function(input,output,session,dms_token) {

  options(shiny.maxRequestSize = 30 * 1024^2)
  #获取参数
  text_apPayable_upload = tsui::var_file('text_apPayable_upload')

  shiny::observeEvent(input$btn_apPayable_upload,{

    filename=text_apPayable_upload()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      # 清空临时表

      mdlDFapPayableUploadPkg::apPayable_delete(dms_token = dms_token)


      data <- readxl::read_excel(filename,col_types = c("text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text"))



      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::mysql_writeTable2(token = dms_token,table_name = 'rds_erp_byd_src_t_ap_payable_list_input',r_object = data,append = TRUE)


      # 插入list表和表头表体

      mdlDFapPayableUploadPkg::apPayable_insert(dms_token = dms_token)

      tsui::pop_notice("上传成功")


    }


  })



}



#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' apPayableViewServer()
apPayableViewServer <- function(input,output,session,dms_token) {

  #获取参数
  text_apPayable_daterange = tsui::var_dateRange('text_apPayable_daterange')

  shiny::observeEvent(input$btn_apPayable_view,{

    FDate = text_apPayable_daterange()

    FStartDate = FDate[1]

    FEndDate = FDate[2]

    data = mdlDFapPayableUploadPkg::apPayable_select(dms_token = dms_token,FStartDate =FStartDate ,FEndDate = FEndDate)

    tsui::run_dataTable2(id = 'apPayable_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_apPayable',data = data,filename = 'BYD应付单.xlsx')




  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' apPayableServer()
apPayableServer <- function(input,output,session,dms_token) {

  apPayableUploadServer(input = input,output = output,session = session,dms_token = dms_token)



  apPayableViewServer(input = input,output = output,session = session,dms_token = dms_token)


}
