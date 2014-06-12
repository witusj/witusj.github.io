---
title       : Quizii
subtitle    : A quiz application build in Shiny
author      : Witek ten Hove
job         : Instructor
framework   : html5slides   # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax, bootstrap, quiz]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
logo        : Logo_short.png

--- &radio

## 1. A quiz in Slidify

A simple quiz question: $sin(2pi)=$

1. _0_
2. 1
3. -1
4. $\infty$

*** .hint

![alt text](assets/img/unit.png)

*** .explanation

The sinus is the vertical distance of a point on a unit circle.

---

## 2. Pretty neat but ...


* Can we get a random question from a set of questions?

--- &radio

## 3. Let's see ...


A sampled quiz question: What is the capital of Love?

1. _Paris, mon Dieu!_
2. London, you fool!
3. Berlin, ohnehin!
4. My place, cherry!

*** .hint

Another croissant, ma cherie?

*** .explanation

Does Love need a place?

---

## 4. So, yes we can!

* But can we randomize the order of the answers?
* Not in Slidify

---

## 5. Enter Shiny!

Shiny has the interactivity and the quiz elements (like radio buttons)

With some simple R code you can sample from a set of questions and return the answers in random order.



A scrambled quiz question: What is the capital of Love?

1. Berlin, ohnehin!
2. Paris mon Dieu!
3. London, you fool!
4. My place, cherry!
