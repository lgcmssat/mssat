shinyUI(navbarPage(strong("LGC Application"),inverse=TRUE,collapsable=TRUE,
  tabPanel(strong("Home"),
	fluidRow(
    column(12,
	br(),
	br(),
	br(),

	h1(div(strong("基于标记的材料间相似性评估工具包"),style = "color:firebrick"),align = "center"),
	h1(strong("Marker-based inter-Sample Similarity Assessment Toolkit",span("(MSSAT)",style = "color:firebrick")),align = "center"),
	br(),
	br(),
	br(),
	h3(div("MSSAT可以做一下分析：",style = "color:firebrick"),align = "center"),
	br(),

	h2(div("*单个材料内有效标记的统计",style = "color:firebrick"),align = "center"),
	h2(div("*单个材料与多个材料间的相似性分析",style = "color:firebrick"),align = "center"),
	h2(div("*多个材料间的相似性分析及相似性网络",style = "color:firebrick"),align = "center"),
	br(),
	br(),
	br(),
	br(),
	br(),
	br(),
	br(),
	br(),
	h5(div(strong("MSSAT是一个基于R软件及附属软件包开发的一个可视化工具包，此软件可以评估材料间的相似性，结果以图和表的形式展示，并且图和表支持下载保存。",style = "color:firebrick")),align = "center")
	
	)
	)
	),
	


	
tabPanel(strong("数据导入"),
headerPanel("导入数据"),

sidebarPanel(
    fileInput('file1',  div('选择 .CSV 格式文件',style = "color:DarkCyan"),
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
								 tags$hr(),
     checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Tab='\t'),
                   ','),
	br(),
	br(),
downloadButton('downloadData1', '下载分析结果')
			
  ),

  # Show a plot of the generated distribution
  mainPanel(
      h3("数据预览"),
      tableOutput('contents'),
	  h3("标记信息统计"),
	  plotOutput("distPlot"),
	  br(),
	   br(),

		  
	  h3(textOutput("text6")),
	  br(),
	  fluidRow(
    DT::dataTableOutput("table1")
  ),
  br(),
		 br()
	)),
	
	
	
	
##########################################################################
tabPanel(strong("相似性分析"),
	
  titlePanel(
  textOutput("text2")
  ),
  sidebarLayout(
    sidebarPanel(
      
      
				   
	  fluidRow(
      textInput("samplename", label = h3("请输入要分析的材料名称，如A1"),
        value = "A1")),
	  div(textOutput("text3"),style = "color:DarkCyan"),
br(),	  
      submitButton("Submit"),

br(),
br(),
div(textOutput("text4"),style = "color:DarkCyan"),
downloadButton('downloadData', 'Download')	  
				   
				   
				   
							   
     
    ),
    mainPanel(
	 h3(textOutput("text1")),
	  #h1("本数据包含的材料名称如下"),
	 # tableOutput('contents2'),
	  plotOutput("distPlot2"),
	  br(),
	   br(),
	    br(),
		 br(),
		  
	  h3(textOutput("text5")),
	  br(),
	  fluidRow(
    DT::dataTableOutput("table2")
  ),
   br(),
    br(),
	
	  h4(div("请点击“Download”下载此表格", style = "color:DarkCyan")),
	  br(),
	   br()
    )
  )),
  
  
  
 tabPanel(strong("网络分析"),
	
  titlePanel( textOutput("text7")
 
 ),

    sidebarPanel(fileInput('file2',  div('选择 .CSV 格式文件',style = "color:DarkCyan"),
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
								 
     
	br(),
	br(),
downloadButton('classinfo', '下载分类结果')
  
   ),
    mainPanel(
	forceNetworkOutput("force"),
	plotOutput("class"),
	br(),
	br()
	)),
	navbarMenu(strong("帮助"),
    tabPanel("软件使用",
	h1(div(strong("<(￣︶￣)>  欢迎使用本软件 <(￣︶￣)> "),style = "color:firebrick"),align = "center"),
	h3(div(strong("数据格式要求"),style = "color:firebrick")),
	h4(div("1、要求输入数据格式为.csv格式。")),
	h4(div("2、每行为一个材料，每列为一个标记。")),
	h4(div("3、缺失标记可以用“NA”或者“?”表示，非缺失标记请用“X:X”形式表示，X可以是字母或者数字。")),
	br(),
	h3(div(strong("关于图表下载"),style = "color:firebrick")),
	h4(div("1、表格下载使用下载按钮进行下载。")),
	h4(div("2、图片下载可使用图片另存为下载。")),
		br(),
			br(),
	h3(div(strong("原理及算法"),style = "color:firebrick")),
	h4(div("1、有效标记是指在这个位点有基因型数据的标记。此部分统计会自动统计除用“NA”或者“？”表示的标记。若缺失值用其它符号表示，如“Na”，则可能高估有效标记的个数及后面分析的错误。")),
	h4(div("2、相似性计算的原理是逐一计算两个材料在同一位点标记的一致性。如“A:A”对“A:A”相似性为1，“A:A”对“A:T”相似性为0.5，“A:A”对“T:T”相似性为0。材料间的相似性为各标记相似性的均值。")),
	h4(div("3、关于相似性网络构建中的材料分组，在每组内材料间的标记是100%一致的，组间各不相同。组间的相似性计算方法同两材料间的相似性计算方法。当组间的相似性达到75%后会构建相似性网络，达不到75%则无网络图输出。"))
	
	),
    tabPanel("关于软件",
	h3(div(strong("关于本软件"),style = "color:firebrick")),
	h4(div("本软件包是基于",a("R: The R Project for Statistical Computing",href ="https://www.r-project.org/"), "和",a("shiny: A web application framework for R",href ="http://shiny.rstudio.com/"),"开发的。")),
	h4(div(strong("本软件包中使用的其它的 R 包还:"))),
	h4(a("ggplot2: elegant graphics for data analysis",href ="http://ggplot2.org/")),
	h4(a("DT: An R interface to the DataTables library",href ="http://rstudio.github.io/DT/")),
	h4(a("networkD3：D3 JavaScript Network Graphs from R",href ="http://christophergandrud.github.io/networkD3/")),
	br(),
	br(),	
	h5(div(strong("反馈"),style = "color:firebrick")),
	h5("如欲要询问软件细节及反馈软件错误，请发邮件至",span("597551036@qq.com。",style = "color:blue"))
	))
))
