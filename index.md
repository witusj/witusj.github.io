---
title       : Shiny in Slidify
subtitle    : See how it works
author      : Witek ten Hove
job         : Instructor
framework   : html5slides        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [shiny, bootstrap, interactive]  # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}


--- 

## Slide 2


```r
slidifyUI(
  sidebarPanel(uiOutput('ui')
         ),
  textOutput('result')
  )
```

```
## Error: could not find function "slidifyUI"
```




