# QUICK AND DIRTY HASKELL PARSING LIBRARY FOR TOKI PONA
this is a parsing library that I created for toki pona because it is cool. 
it reflects largely the way that I parse toki pona myself.

## HWÆT TO EXPECT
* branching parser that reflects multiple different parsings of syntactically ambiguous situations
* janky novice haskell code
* gaming

## HWÆT NOT TO EXPECT
* completeness
* buglessness

## SAMPLE
here are the possible parsings given for "tomo tawa ona li tawa ala tawa ona"
```
(Li [NP "tomo" [Pr (Prep "tawa" (NP "ona" []))]] [Predicate (Question (VP (NP "" [Pr (Prep "tawa" (NP "ona" []))]))) [] []],"")
(Li [NP "tomo" [Pr (Prep "tawa" (NP "ona" []))]] [Predicate (VP (NP "" [Pr (PPAla "tawa" (NP "tawa" [Single "ona"]))])) [] []],"")
(Li [NP "tomo" [Pr (Prep "tawa" (NP "ona" []))]] [Predicate (VP (NP "" [Pr (Prep "tawa" (NP "ala" [Pr (Prep "tawa" (NP "ona" []))]))])) [] []],"")
(Li [NP "tomo" [Pr (Prep "tawa" (NP "ona" []))]] [Predicate (VP (NP "" [Pr (Prep "tawa" (NP "ala" [Single "tawa",Single "ona"]))])) [] []],"")
(Li [NP "tomo" [Pr (Prep "tawa" (NP "ona" []))]] [Predicate (Question (VP (NP "tawa" [Single "ona"]))) [] []],"")
(Li [NP "tomo" [Pr (Prep "tawa" (NP "ona" []))]] [Predicate (VP (NP "tawa" [Single "ala",Pr (Prep "tawa" (NP "ona" []))])) [] []],"")
(Li [NP "tomo" [Pr (Prep "tawa" (NP "ona" []))]] [Predicate (VP (NP "tawa" [Single "ala",Single "tawa",Single "ona"])) [] []],"")
(Li [NP "tomo" [Single "tawa",Single "ona"]] [Predicate (Question (VP (NP "" [Pr (Prep "tawa" (NP "ona" []))]))) [] []],"")
(Li [NP "tomo" [Single "tawa",Single "ona"]] [Predicate (VP (NP "" [Pr (PPAla "tawa" (NP "tawa" [Single "ona"]))])) [] []],"")
(Li [NP "tomo" [Single "tawa",Single "ona"]] [Predicate (VP (NP "" [Pr (Prep "tawa" (NP "ala" [Pr (Prep "tawa" (NP "ona" []))]))])) [] []],"")
(Li [NP "tomo" [Single "tawa",Single "ona"]] [Predicate (VP (NP "" [Pr (Prep "tawa" (NP "ala" [Single "tawa",Single "ona"]))])) [] []],"")
(Li [NP "tomo" [Single "tawa",Single "ona"]] [Predicate (Question (VP (NP "tawa" [Single "ona"]))) [] []],"")
(Li [NP "tomo" [Single "tawa",Single "ona"]] [Predicate (VP (NP "tawa" [Single "ala",Pr (Prep "tawa" (NP "ona" []))])) [] []],"")
(Li [NP "tomo" [Single "tawa",Single "ona"]] [Predicate (VP (NP "tawa" [Single "ala",Single "tawa",Single "ona"])) [] []],"")
```
that sentence is indeed pretty ambiguous.

## LICENSE
it's GPL, my friend.
