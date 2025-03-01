shinyServer(function(input, output, session) {
  showModal(modalDialog(
    title="Stable release (Version 1.0)", 
    "This version of the ReLiSyR app uses a snapshot of our database from 1 April 2021 to accompany our publication describing our systematic approach in selecting the 3rd arm of MND-SMART. The data presented here are not kept up to date and should not be used to drive future decisions."
  ))


    #--------
    output$header <- renderUI({
      h4("Disclaimer: This stable release (version 1.0) of the ReLiSyR app uses a snapshot of our database from 1 April 2021 to accompany our publication describing our systematic approach in selecting the 3rd arm of MND-SMART. The data presented here are not kept up to date and should not be used to drive future decisions.")
      
    })
    #ui body----
    output$body<- renderUI({
      navbarPage(
        "ReLiSyR 1.0",
        
        tabPanel(
          "Home",
          
          
          
          column(3,
                 selectizeInput(
                   "chosenDiseasesReLiSyR",
                   "Choose a disease to apply ReLiSyR logic"
                   ,
                   c("All Diseases",diseaseOfInterest),
                   multiple = F,
                   selected = "Not chosen"
                 ),      
                 br(), br(),
                 materialSwitch(inputId = "OnlyCandidates", label = "Only show MND longlisted drugs", value = FALSE, status = "info"),
                 br(),hr(),
                 downloadButton("DownloadFilteredPublications", label = "Clinical publications for filtered drugs", class = "btn-info"),
                 br(),br(),
                 downloadButton("DownloadSelectedPublications", label = "Clinical publications for selected drugs", class = "btn-primary"),
                 hr(),
                 downloadButton("DownloadFilteredPublicationsinvivo", label = "in vivo publications for filtered drugs", class = "btn-info"), 
                 br(),br(),
                 downloadButton("DownloadSelectedPublicationsinvivo", label = "in vivo publications for selected drugs", class = "btn-primary"),  
                 br(),br(),
                 p("Data shown are from a snapshot on April 2021.")
          ),
          column(9,
                 tabsetPanel(type = "tabs",
                             tabPanel("Drug Disease Cross Table",   DT::dataTableOutput("frequencyCrossTable") %>% withSpinner(type = 5, color = "#2c3e50")),
                             tabPanel("Study List",   DT::dataTableOutput("studyTable") %>% withSpinner(type = 5, color = "#2c3e50"))
                 )
          )),
        tabPanel(
          "About",
          
          
          fluidRow(
            h3("About this app"),
            p('This stable release (version 1.0) of the ReLiSyR app uses a snapshot of our database from 1 April 2021 to accompany our publication describing our systematic approach in selecting the 3rd arm of',
              tags$a(href="https://mnd-smart.org","Motor Neurone Disease – Systematic Multi-arm Adaptive Randomised Trial (MND-SMART)", .noWS = "after"),
              ". MND-SMART is registered on ClinicalTrials.gov",
              tags$a(href="https://www.clinicaltrials.gov/ct2/show/NCT04302870", "(NCT04302870)"),
              "and the European Clinical Trials Registry",
              tags$a(href = "https://www.clinicaltrialsregister.eu/ctr-search/search?query=2019-000099-41", "(2019-000099-41)", .noWS = "after"),
              
              '. As part of our systematic and structured approach in prioritisation and selection of candidate drugs, we began with summarising published data for drugs tested in neurodegenerative diseases using the Repurposing Living Systematic Review - MND (ReLiSyR-MND). Briefly, we perform a living search of clinical studies of MND and other neurodegenerative diseases (Alzheimer dementia, frontotemporal dementia (FTD), Huntington disease, Parkinson disease and multiple sclerosis) which may share similar pathways; animal in vivo studies of MND and FTD models; and in vitro studies of MND and FTD models. 
               Using a machine learning algorithm, citations are screened for inclusion based on title and abstract. We use text mining to annotate included publications for drug and disease studied. 
              This app summarises all interventions described in included clinical and in vivo publications. The main table shows interventions identified and the number of clinical publication for each intervention across diseases of interest. 
              We apply the "ReLiSyR" logic to prioritise drugs, taking forward drugs described in at least one clinical publication in MND OR in clinical publications in two other diseases of interest. 
              The MND-SMART drug selection group reviewed these drugs and longlisted drugs listed in the British National Formulary, prescription-only medicine available in oral formulation and not previously in the 2017 ‘red list’ during selection of the first two arms of MND-SMART, deemed by investigators to be appropriate for MND-SMART.
              '),
            
            p("We have since added consideration of domains describing findings from in vitro high throughput phenotypic drug screening, pathway and network analysis, and mining of drug, compound, and clinical trial databases.We generate an integrated candidate drug list using these domains and filter drugs with our prioritisation criteria. Our expert panel longlist drugs from the filtered list. For longlisted drugs, we generate, evaluate, and synthesise further evidence across domains. We report living evidence summaries using an interactive web application which can be explored by the expert panel to inform their decisions on drug shortlisting and selection at trial adaptation boundaries."),
            
            p("More information is available in our preprint."),
            
            
            h3("ReLiSyR-MND"),
            p("Repurposing Living Systematic Review-MND (ReLiSyR-MND) is a three-part machine learning assisted living systematic review of:",
              tags$ul(
                tags$li("Clinical literature of MND and other neurodegenerative diseases which may share common pivotal pathways, namely, Alzheimer's disease (AD), Frontotemporal dementia (FTD), Huntington's disease (HD), Multiple Sclerosis (MS) and Parkinson's disease (PD)."),
                tags$li("Animal in vivo literature of MND and FTD models."),
                tags$li("In vitro studies of MND and FTD models including induced pluripotent stem cell studies.")
              )),
            p("Our methodology is detailed in our protocol deposited on the Open Science Framework",
              tags$a(href="https://mfr.de-1.osf.io/render?url=https://osf.io/bkscj/?direct%26mode=render%26action=download%26mode=render", "(https://osf.io/bkscj)", .noWS = "after"),
              ". We adopted a systematic approach which we have previously used to guide drug selection for Multiple Sclerosis Secondary Progressive Multi-Arm Randomisation Trial (MS-SMART), a multi-arm phase IIb randomised controlled trial comparing the efficacy of three neuroprotective drugs in secondary progressive multiple sclerosis",
              tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0117705","(Vesterinen 2015)"),
              
              "and for the first two treatment arms of MND-SMART",
              tags$a(href = "https://www.medrxiv.org/content/10.1101/2022.04.13.22273823v2", "(Wong 2022)", .noWS = "after"), "."),
            
            p("This approach, which adopts a structured, systematic method combined with independent expert evaluation, was designed to identify candidate drugs for evaluation in clinical trials for people with neurodegenerative diseases, including MND, on account of the compelling evidence for shared dysregulated pathways and processes across neurodegenerative disorders.
                     Critically, the structured evaluation takes into account not only biological plausibility and efficacy but also safety and quality of previous studies. This includes adopting benchmark practice such as Delphi and PICOS framework."),
            p("1.", strong("Living Search"),
              ": We use the",
              tags$a(href="https://syrf.org.uk","Systematic Review Facility (SyRF) platform", .noWS = "after"),
              ", taking as its starting point automatic updating of the PubMed search."),
            p("2.", strong("Citation Screening"),
              ": Using a machine learning algorithm which has been trained and validated using human decisions, publications are screened for inclusion based on title and abstract."),
            p("3.", strong("Filtering drugs by inclusion logic"),
              ": Text mining approaches (Regular Expressions deployed in R and taking as source material title and abstract) are used to identify disease and drug studied. A second algorithm is used to identify drugs with at least one clinical publication in MND OR clinical publications in two or more other diseases of interest."),
            p("4.", strong("Longlisting by trial investigators"),
              ": Trial investigators reviewed the drugs filtered, excluding drugs which met the following criteria: (i) previously considered unsuitable by expert panel due to lack of biological plausibility, drugs with unfavourable safety profiles in MND patients and drugs tested more than 3 times in MND population; (ii) drugs available over-the-counter as these may affect trial integrity; (iii) compounds which are not feasible for the next arms due to supply issues, such as compopunds not listed in the current version of the British National Formulary; (iv) drugs without oral preparations; and (v) drugs that are deemed by investigators to be unsafe/inappropriate for clinical trial in the current setting."),
            p("5.", strong("Data extraction"),
              ": Using the",
              tags$a(href="https://syrf.org.uk", "SyRF platform", .noWS = "after"),
              ", our team of reviewers extract data from included publications for longlisted drugs. Each publication will be annotated by at least two reviewers, with discrepancies reconciled by a third reviewer."),
            p("6.", strong("Data Analysis"),
              ": We will analyse the results as follows:",
              tags$ul(
                tags$li("Clinical review:","For each publication, we calculate a distance score based on Euclidean distance of efficacy and safety scores weighted by quality and study size. For each drug, we calculate a drug score using the number of publications describing the drug (n) and median publication distance score for all publications describing data for that drug:", withMathJax("$$\\text{drug score}\\ = log10{(n+1)} \\times {(\\text{median distance score})}$$"),
                        
                        "Separately, we will calculate median subscores for efficacy, safety, quality and study size across all publications for each drug.",
                        tags$br(),
                        "Further details are available on",
                        tags$a(href = "https://mfr.de-1.osf.io/render?url=https://osf.io/8k4h2/?direct%26mode=render%26action=download%26mode=render", "https://osf.io/8k4h2", .noWS = "after"),
                        "."
                ),
                tags$li("Animal in vivo review and in vitro review: An individual meta‐analysis will be carried out for each intervention identified. We will summarise the effects of interventions where there are 3 or more publications in which that intervention has been tested reporting findings from at least 5 experiments. Depending on the nature of the outcomes reported we will use either standardised mean difference (SMD) or normalised mean difference (NMD) random effects meta-analysis with REML estimates of tau. Specifically, if fewer than 70% of outcomes are suitable for NMD analysis we will use SMD. Differences between groups of studies will be identified using meta-regression."
                ))
            ),
            
            h3("MND-SMART"),
            img(src = "MND-SMART-logo.png", height = 100, align = "center"),
            
            p("We aim to provide a living summary of evidence to identify, evaluate and prioritise candidate drugs for", 
              tags$a(href="https://mnd-smart.org","Motor Neurone Disease – Systematic Multi-arm Adaptive Randomised Trial (MND-SMART)", .noWS = "after"),
              ". MND-SMART is registered on ClinicalTrials.gov",
              tags$a(href="https://www.clinicaltrials.gov/ct2/show/NCT04302870", "(NCT04302870)"),
              "and the European Clinical Trials Registry",
              tags$a(href = "https://www.clinicaltrialsregister.eu/ctr-search/search?query=2019-000099-41", "(2019-000099-41)", .noWS = "after"),
              ". MND-SMART uses a multi-arm multi-stage adaptive trial design to speed up the time it takes to find medicines that can slow, stop, or reverse the progression of MND.
        Multi-arm means that, unlike typical clinical trials which test a single treatment, MND-SMART will test more than one at the same time. Trial participants taking the different treatments will be compared with a single group who receive a dummy drug, called a placebo. This means that people in MND-SMART are more likely to receive an active treatment when compared to standard clinical trials where half of participants receive the placebo and half the active treatment.
         With the multi-stage, adaptive design, researchers can change the drugs being tested according to emerging results. This means that new medicines can be added once a trial has started, while treatments that do not prove effective can be dropped."),
            
            p("For more information on the trial design, please see our",
              tags$a(href="https://bmjopen.bmj.com/content/12/7/e064173", "trial protocol publication in BMJ Open", .noWS = "after"),
              ".", 
            ),
            
            p(em("Wong C, Dakin RS, Williamson J, et al Motor Neuron Disease Systematic Multi-Arm Adaptive Randomised Trial (MND-SMART): a multi-arm, multi-stage, adaptive, platform, phase III randomised, double-blind, placebo-controlled trial of repurposed drugs in motor neuron diseaseBMJ Open 2022;12:e064173. doi: 10.1136/bmjopen-2022-064173")),
            p(
              "MND-SMART is led by the",
              tags$a (href="http://euanmacdonaldcentre.org/", "Euan MacDonald Centre"),
              "based at the",
              tags$a(href="https://www.ed.ac.uk/", "University of Edinburgh"),
              "alongside colleagues from",
              tags$a (href="https://www.ucl.ac.uk/", "University College London"),
              "and the",
              tags$a (href="https://warwick.ac.uk/", "University of Warwick", .noWS = "after"),
              ". The trial receives funding from the",
              tags$a(href="http://euanmacdonaldcentre.org/", "Euan MacDonald Centre for MND Research", .noWS = "after"),
              ",",
              tags$a(href="https://www.mndscotland.org.uk/", "MND Scotland"), 
              "and",
              tags$a(href="https://www.myname5doddie.co.uk/", "My Name'5 Doddie Foundation", .noWS = "after"),
              "."
            ),
            
            h3("CAMARADES"),
            img(src = "Camarades Logo.png", height = 80, align = "center"),
            
            tags$br(),
            tags$br(),
            
            p("The", tags$a(href="https://www.ed.ac.uk/clinical-brain-sciences/research/camarades/", "CAMARADES"), "(Collaborative Approach to Meta-Analysis and Review of 
                     Animal Data from Experimental Studies) research group was founded in 2004 by Malcolm Macleod and David Howells, aiming to address translational failures in preclinical stroke research using systematic review and meta-analysis. Since then, the CAMARADES have broadened their scope to other disease models such as neuropathic pain and Alzheimer's disease.
The CAMARADES has pioneered in the field of preclinical systematic review, and in addition to carrying out their own research, provide a supportive framework for other research groups performing systematic review."),
            p("CAMARADES have recently developed",  strong("Systematic Online Living Evidence Summaries (SOLES)"), "to accelerate evidence synthesis. In this approach, we integrate automated processes to continuously gather, synthesise and summarise all existing evidence from a research domain, and report the resulting current curated content as interrogatable databases via interactive web applications. ", "Other SOLES projects include ", 
              tags$a(href="https://camarades.shinyapps.io/AD-SOLES", "animal models of Alzheimer's Disease", .noWS = "after"), ", ",
              tags$a(href="https://camarades.shinyapps.io/COVID-19-SOLES", "COVID-19", .noWS = "after"), ", and",
              tags$a(href="https://camarades.shinyapps.io/SPRINT-SOLES/", "impacts of pesticides on environment and human health", .noWS = "after"),
              ". For more information on SOLES, please see our preprint on MetaArXiv:",
              tags$a(href="https://osf.io/preprints/metaarxiv/nbe5q/", "https://osf.io/preprints/metaarxiv/nbe5q/", .noWS = "after"),
              "."
            ),
            
            h3("Code"),
            p("Source code for relisyr 1.0 is available on",
              tags$a(href = "https://github.com/charis-wong/relisyr-v1.0", "GitHub", .noWS = "after"),
              "."),
            
            h3("Contact us"),
            p("For queries, please contact", 
              tags$a(href = "mailto:charis.wong@ed.ac.uk", 
                     "charis.wong@ed.ac.uk", .noWS = "after"),
              "."
            )
 
          )
        )
      )
    })
    #----
    
    
    
    #server----
    
    outputCrossTable <- reactive({
      clinicalStudyList$Disease <- factor(clinicalStudyList$Disease, levels = diseaseOfInterest)
      clinicalOutputCrossTable <- as.data.frame.matrix(table(clinicalStudyList[,c("Drug","Disease")]))
      
      return(clinicalOutputCrossTable[, diseaseOfInterest])
    })
    
    filteredDrugs <-  reactive({
      myOutputCrossTable <- outputCrossTable()
      filteredOutputCrossTable <- myOutputCrossTable
      
      if(input$chosenDiseasesReLiSyR != "All Diseases"){
        myOutputCrossTable$select1  <- F
        for(chosenDiease in input$chosenDiseasesReLiSyR){
          myOutputCrossTable$score11 <- rowSums(myOutputCrossTable[, chosenDiease, drop = F])
          myOutputCrossTable$score12 <- rowSums(myOutputCrossTable[, setdiff(diseaseOfInterest, chosenDiease), drop=F] > 0)
          myOutputCrossTable$select1 <- myOutputCrossTable$select | (myOutputCrossTable$score11 > 0 | myOutputCrossTable$score12 >= 2)
        }
        filteredOutputCrossTable1 <- myOutputCrossTable[which(myOutputCrossTable$select1), ]
      } else{
        filteredOutputCrossTable1 <- myOutputCrossTable
      }
      chosenDrugs <- rownames(filteredOutputCrossTable1)
      
      if(input$OnlyCandidates)  chosenDrugs <- intersect(drugOfInterest, chosenDrugs)
      
      return(chosenDrugs)
    })
    
    frequencyCrossTable <- reactive({
      myOutputCrossTable <- outputCrossTable()
      filteredDrugs <- filteredDrugs()
      return(myOutputCrossTable[filteredDrugs, ])
    })
    
    output$studyTable <- DT::renderDataTable(DT::datatable({
      myTable <-  filiteredPublicationTable()
      myTable$Title <- paste0(myTable$Title, "(",myTable$Author,")")
      myTable$Title <- paste0("<a href='",myTable$Link ,"'target='_blank'>" , myTable$Title,"</a>" )
      
      index <- which(names(myTable) %in% c("X","Journal","Abstract","OldIdStr", "idStr", "Author","Link"))
      return(   
        myTable[,-index]
      )
    }) ,extensions = 'Buttons'
    , filter = 'top', options = list(
      pageLength = 10,lengthMenu = list(c(10,25,50,100,1000, -1), c("10", "25", "50", "100", "1000", "All")),autoWidth = TRUE
    ), escape=F) 
    
    output$frequencyCrossTable <- DT::renderDataTable(DT::datatable({
      return(    frequencyCrossTable())
    }), 
    filter = 'top', options = list(
      pageLength = 50,lengthMenu = list(c(10,25,50,100,1000, -1), c("10", "25", "50", "100", "1000", "All"))
      , dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    extensions = c("Buttons", "Responsive"))
    
    filiteredPublicationTable <- reactive({
      myOutputCrossTable <- frequencyCrossTable()
      chosenDrugs <- filteredDrugs()
      chosenStudies <- clinicalStudyList[clinicalStudyList$Drug %in% chosenDrugs,] %>%
        group_by(OldIdStr) %>%
        summarise(Title = first(Title),
                  Author = first(Author),
                  Journal = first(Journal),
                  Abstract = first(Abstract),
                  Year = first(Year),
                  idStr = first(idStr),
                  Disease = paste0(unique(Disease), collapse = "; "),
                  Drug = paste0(unique(Drug), collapse = "; "),
                  MSType = paste0(unique(MSType), collapse = "; "))%>%
        mutate(Link = paste0("https://syrf.org.uk/projects/3239dadc-181d-4325-a4be-be89f0613175/stage/2c400348-d871-4055-9c68-2bc529ac9ccc/review/",
                             idStr))
      return(chosenStudies)
    })
    
    output$DownloadFilteredPublications <-  downloadHandler(
      filename = "FilteredPublications.csv", content = function(file){
        write.csv({
          filiteredPublicationTable()
        }
        , file, na = "", row.names = F
        )
      })
    
    selectedPublicationTable <- reactive({
      myOutputCrossTable <- frequencyCrossTable()
      chosenDrugs <- rownames(myOutputCrossTable)[input$frequencyCrossTable_rows_selected]
      chosenStudies <- clinicalStudyList[clinicalStudyList$Drug %in% chosenDrugs,] %>%
        group_by(OldIdStr) %>%
        summarise(Title = first(Title),
                  Author = first(Author),
                  Journal = first(Journal),
                  Abstract = first(Abstract),
                  Year = first(Year),
                  idStr = first(idStr),
                  Disease = paste0(Disease, collapse = "; "),
                  Drug = paste0(Drug, collapse = "; "),
                  MSType = paste0(MSType, collapse = "; "))%>%
        mutate(Link = paste0("https://syrf.org.uk/projects/3239dadc-181d-4325-a4be-be89f0613175/stage/2c400348-d871-4055-9c68-2bc529ac9ccc/review/",
                             idStr))
      return(chosenStudies)
    })
    
    output$DownloadSelectedPublications <-
      downloadHandler(
        filename = "SelectedPublications.csv",
        content = function(file){
          write.csv({
            selectedPublicationTable()
          }
          , file, na = "", row.names = F
          )
        }
      )
    
    filiteredPublicationTableinvivo <- reactive({
      chosenDrugs <- filteredDrugs()
      choseninvivoStudies <- invivoStudyList[invivoStudyList$Drug %in% chosenDrugs,] %>%
        group_by(idStr) %>%
        summarise(Title = first(Title),
                  Author = first(Author),
                  Journal = first(Journal),
                  Abstract = first(Abstract),
                  Year = first(Year),
                  idStr = first(idStr),
                  Disease = paste0(Disease, collapse = "; "),
                  Drug = paste0(Drug, collapse = "; ")
        )%>%
        mutate(Link = paste0("https://syrf.org.uk/projects/98905f6a-ed99-473e-bc82-1b52b95181f4/stage/18fd37ba-35f6-4ddb-91d1-9f8a69d31f0e/review/", idStr))
      
      return(choseninvivoStudies)
    })
    
    output$DownloadFilteredPublicationsinvivo <-  downloadHandler(
      filename = "invivoFilteredPublications.csv", content = function(file){
        write.csv({
          filiteredPublicationTableinvivo()
        }
        , file, na = "", row.names = F
        )
      })
    
    selectedPublicationTableinvivo <- reactive({
      myOutputCrossTable <- frequencyCrossTable()
      
      chosenDrugs <- rownames(myOutputCrossTable)[input$frequencyCrossTable_rows_selected] 
      choseninvivoStudies <- invivoStudyList[invivoStudyList$Drug %in% chosenDrugs,] %>%
        group_by(idStr) %>%
        summarise(Title = first(Title),
                  Author = first(Author),
                  Journal = first(Journal),
                  Abstract = first(Abstract),
                  Year = first(Year),
                  idStr = first(idStr),
                  Disease = paste0(Disease, collapse = "; "),
                  Drug = paste0(Drug, collapse = "; ")
        )%>%
        mutate(Link = paste0("https://syrf.org.uk/projects/98905f6a-ed99-473e-bc82-1b52b95181f4/stage/18fd37ba-35f6-4ddb-91d1-9f8a69d31f0e/review/", idStr))
      
      return(choseninvivoStudies)
    })
    
    output$DownloadSelectedPublicationsinvivo <-
      downloadHandler(
        filename = "invivoSelectedPublications.csv",
        content = function(file){
          write.csv({
            selectedPublicationTableinvivo()
          }
          , file, na = "", row.names = F
          )
        }
      )

})
