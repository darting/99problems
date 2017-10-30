module Tests.Helper

open Xunit

let equal<'T when 'T : equality> (a : 'T) (b : 'T) = 
    // Assert.Equal (b, a)
    if a = b then ()
    else failwithf "\r\n Expected: %A \r\n Actual: %A" b a

let notEqual<'T> (a : 'T) (b : 'T) = Assert.NotEqual (b, a)

let (<==>) a b = equal a b

let (<!=>) a b = notEqual (b, a)

let isTrue (x : bool) = Assert.True x

let isFalse (x : bool) = Assert.False x
