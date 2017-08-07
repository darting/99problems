module Tests.Helper

open Xunit

let equal<'T> (a : 'T) (b : 'T) = Assert.Equal (b, a)

let notEqual<'T> (a : 'T) (b : 'T) = Assert.NotEqual (b, a)

let (=?=) a b = equal a b

let (=/=) a b = notEqual (b, a)

let isTrue (x : bool) = Assert.True x

let isFalse (x : bool) = Assert.False x
