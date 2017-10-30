module Tests.WorkingWithLists

open System
open Xunit
open Tests.Helper

[<Fact>]
let ``Write a function last : 'a list -> 'a option that returns the last element of a list`` () =

    let rec last = function
        | [] -> None
        | [ x ] -> Some x
        | _ :: x -> last x

    last [ "a" ; "b" ; "c" ; "d" ] <==> Some "d"
    last [ ] <==> None

[<Fact>]
let ``Find the last but one (last and penultimate) elements of a list`` () =
    let rec last_two = function
        | [] | [ _ ] -> None
        | [ x; y ] -> Some (x, y)
        | _ :: x -> last_two x

    last_two [ "a" ; "b" ; "c" ; "d" ] <==> Some ("c", "d")
    last_two [ "a" ] <==> None

[<Fact>]
let ``Find the k'th element of a list.`` () =
    let rec at i l =
        match i, l with
        | _, _ when i < 1 -> None
        | _, [ ] -> None
        | 1, h :: t -> Some h
        | i, h :: t -> at (i - 1) t

    at 3 [ "a" ; "b"; "c"; "d"; "e" ] <==> Some "c"
    at 3 [ "a" ] <==> None 

[<Fact>]
let ``Find the number of elements of a list`` () =
    let length = 
        let rec helper i = function
            | [ ] -> i
            | _ :: t -> helper (i + 1) t
        helper 0

    length [ "a" ; "b" ; "c"] <==> 3
    length [] <==> 0

[<Fact>]
let ``Reverse a list`` () =
    let rev = 
       let rec helper s = function
           | [ ] -> s
           | h :: t -> helper (h :: s) t
       helper []  
         
    rev ["a" ; "b" ; "c"] <==> ["c"; "b"; "a"]

[<Fact>]
let ``Find out whether a list is a palindrome`` () =
    let is_palindrome l = List.rev l = l

    is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] <==> true
    not (is_palindrome [ "a" ; "b" ]) <==> true

type 'a Node =
        | One of 'a 
        | Many of 'a Node list

[<Fact>]
let ``Flatten a nested list structure. (medium)`` () =
    let flatten l =
        let rec helper s = function
            | [ ] -> s
            | One x :: t -> helper (x :: s) t
            | Many x :: t -> helper (helper s x) t
        helper [ ] l |> List.rev
    flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] <==> ["a"; "b"; "c"; "d"; "e"]

[<Fact>]
let ``Eliminate consecutive duplicates of list elements. (medium)`` () =
    let rec compress list =
        // let rec helper s last l = 
        //     match last, l with
        //     | _, [] -> s
        //     | None, h :: t -> helper (h :: s) (Some h) t
        //     | Some x, h :: t when x = h -> helper s (Some h) t
        //     | _, h :: t -> helper (h :: s) (Some h) t
        // helper [] None list |> List.rev
        match list with
        | a :: (b :: _ as t) ->
            if a = b then compress t else a :: compress t
        | x -> x
    compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] <==> ["a"; "b"; "c"; "a"; "d"; "e"]
    
[<Fact>]
let ``Pack consecutive duplicates of list elements into sublists. (medium)`` () =
    let pack = 
        let rec helper s g = function
            | a :: (b :: _ as t) -> 
                if a = b then helper s (a :: g) t else helper ((a :: g) :: s) [] t
            | [ x ] -> helper ((x :: g) :: s ) [] []
            | [] -> List.rev s
        helper [] []

    pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] <==> 
         [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
    pack [] <==> []
    pack ["a"] <==> [["a"]]
    pack ["a"; "b"] <==> [["a"]; ["b"]]
    pack ["a"; "a"; "b"] <==> [["a"; "a"]; ["b"]]
    pack ["a"; "a"; "a"] <==> [["a"; "a"; "a"]]
    
[<Fact>]    
let ``Run-length encoding of a list. (easy)`` () =
    let encode = 
        let rec helper s i = function
            | [] -> []
            | [ x ] -> (i + 1, x) :: s |> List.rev
            | (a :: (b :: _ as t)) ->
                if a = b then helper s (i + 1) t else helper ((i + 1, a) :: s) 0 t
        helper [] 0

    encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] <==>
            [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

type 'a Rle = 
    | One of 'a
    | Many of int * 'a

[<Fact>]
let ``11. Modified run-length encoding. (easy)`` () =
    let encode src = 
        let rec helper s i = function
            | [] -> []
            | [ x ] -> (i + 1, x) :: s |> List.rev
            | (a :: (b :: _ as t)) ->
                if a = b then helper s (i + 1) t else helper ((i + 1, a) :: s) 0 t
        src
        |> helper [] 0
        |> List.map (fun (a, b) -> if a > 1 then Many (a, b) else One b )
        
    encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
    <==> [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";Many (4, "e") ]

[<Fact>]
let ``12. Decode a run-length encoded list. (medium)`` () =
    let rec decode = function
        | [] -> []
        | x :: xs -> 
            match x with
            | Many (a, b) -> (List.replicate a b) @ decode xs
            | One a -> a :: decode xs
    decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] 
    <==> ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

[
