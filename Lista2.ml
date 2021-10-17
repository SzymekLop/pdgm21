let sum list =
  let rec sumIter(list, sum) =
    if list = [] then sum
    else sumIter(List.tl list,sum+List.hd list)
  in sumIter(list,0)
;;  

sum [1;2;2;3];;
sum [1;1;1;1;1;1;1;1;1;1];;
sum [];;

let sentencer words =
  let rec sentencerIter(words, sentence) =
    if words = [] then sentence ^ "."
    else if List.hd words = "." || List.hd words = "?" || List.hd words = "!" 
    then sentence ^ List.hd words
    else sentencerIter(List.tl words, sentence ^ List.hd words ^ " ")
  in sentencerIter(words,"")
;; 

sentencer ["Ala";"ma";"kota";"."];;
sentencer ["Kto";"tam";"?"];;
sentencer ["Idz";"sobie";"!","tegoniema"];;
sentencer [];;
sentencer ["Ala";"ma";"juz";"dosc"];;

let graterZeroChecker list =
  let rec graterZeroCheckerIter(list, notEmpty) = 
    if list = [] then notEmpty
    else if List.hd list < 0 then false
    else graterZeroCheckerIter(List.tl list, true)
  in graterZeroCheckerIter(list, false)
;;    

graterZeroChecker [1;2;3;4;0];;
graterZeroChecker [];;
graterZeroChecker [-1;-2;3;4];;

let factroial x = 
  let rec factroialIter(x, res) =
    if x = 0 then res
    else if x <= 0 then -1
    else factroialIter(x-1,res*x)
  in factroialIter(x,1)
;;

factroial 4;;
factroial (-6);;
factroial 0;;