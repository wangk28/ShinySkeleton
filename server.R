library(shiny)

shinyServer(function(input, output, session) {
  user <- reactive({
    session$user
  })
  
  # Overview: Mapping links to tabs
  observeEvent(input$link_to_timingexposure, {
    newvalue <- "timingexposure"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_timingbivariateexposure, {
    newvalue <- "timingbivariateexposure"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_timingdeferralcurves, {
    newvalue <- "timingdeferralcurves"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_timingvariableimportance, {
    newvalue <- "timingvariableimportance"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_timingpredictions, {
    newvalue <- "timingpredictions"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_timingAEdeferralcurves, {
    newvalue <- "timingAEdeferralcurves"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_timingAEcommencementrates, {
    newvalue <- "timingAEcommencementrates"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_efficiencyexposure, {
    newvalue <- "efficiencyexposure"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_efficiencypredictions, {
    newvalue <- "efficiencypredictions"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  observeEvent(input$link_to_efficiencyAEprobabilities, {
    newvalue <- "efficiencyAEprobabilities"
    updateTabsetPanel(session, "navbarpanels", newvalue)
    # updateNavbarPage(session,"navbarpanels", newvalue)
  })
  
  Authorized <- reactive({
    if (1 == "Ben.Johnson") {
      return(TRUE)
    } else{
      return(FALSE)
    }
  })
  
  Auth <- isolate(Authorized())
  hide(id = "loading-content")
  # Sys.sleep(0.55)
  if (Auth) {
    show("NotAuthorized")
  }
  else {
    show("UtilRecon")
  }
  
})