exception OFN_type_mismatch
exception Improper_OFN

type family = Trapezoidal | Gaussian | Exponential 

type tofn = {
    ofn_type : family; 
    au: float; 
    bu : float;
    ad : float; 
    bd : float; 
}

let sametype x y = x.ofn_type = y.ofn_type

let tuplemap f x y = {  
    ofn_type = x.ofn_type; 
    au = f x.au y.au; 
    bu = f x.bu y.bu;
    ad =  f x.ad y.ad; 
    bd = f x.bd y.bd;
}

let tuplemap_safe f x y =
    if sametype x y then {
        ofn_type = x.ofn_type;
        au = f x.au y.au;
        bu = f x.bu y.bu;
        ad =  f x.ad y.ad;
        bd = f x.bd y.bd;
    }
    else raise OFN_type_mismatch

let base_function f = 
match f with
    | Trapezoidal -> fun x -> x
    | Gaussian -> fun x -> sqrt(-2. *. log x)
    | Exponential -> fun x -> exp x

let inv f =
match f with 
    | Trapezoidal -> fun x -> x
    | Gaussian -> fun x -> exp (-0.5 *. x**2. )
    | Exponential -> fun x -> log x


(* Arithmetic operations *)
let (|+|) x y = tuplemap_safe (+.) x y

let (|-|) x y = tuplemap_safe (-.) x y 

let (|*|) x y = tuplemap_safe ( *. ) x y

let (|/|) x y = 
    if y.au = 0. || y.bu = 0. || y.ad = 0. || y.bd = 0. 
    then raise Division_by_zero 
    else tuplemap_safe (/.) x y 

let is_increasing x = x.au > 0. 

let is_decreasing x = x.au < 0.

let is_proper x = 
    let sgn a = 
        match a with 
            | 0. -> 0.
            | _ -> ( a /. (abs_float a))
    in
    if sgn x.au = sgn x.ad then false 
    else true

let membership x = 
    if not (is_proper x) then raise Improper_OFN else
    let f = base_function x.ofn_type and fi = inv x.ofn_type in
    
    (* OFNs without compact support must be treated separately. *)
    
    match x.ofn_type with
        | Gaussian | Exponential -> 
            if is_increasing x then fun y ->
                if  y < x.bu then fi ((y -. x.bu) /. x.au)
                else if (y >= x.bu && y <= x.bd) then 1.
                else if y > x.bd then fi ((y -. x.bd) /. x.ad)
                else 0.
            else fun y ->
                if  y < x.bd then fi ((y -. x.bd) /. x.ad)
                else if (y <= x.bd && y >= x.bu) then 1.
                else if y > x.bu then fi ((y -. x.bu) /. x.au)
                else 1.
        | Trapezoidal -> 
            if is_increasing x then fun y -> 
            let x0 = fi (-.x.bu /. x.au) in
            let x1 = fi (1. -. x.bu) /. x.au in
            let x2 = fi (1. -. x.bd) /. x.ad in
            let x3 = fi (-.x.bd /. x.ad) in

                if y >= x0 && y < x1 then x.au *. f y +. x.bu
                else if y >= x1 && y <= x2 then 1.
                else if y > x2 && y <= x3 then x.ad *. f y +. x.bd
                else 0.
            
            else fun y -> 
                let x0 = fi (-.x.bd /. x.ad) in
                let x1 = fi (1. -. x.bd) /. x.ad in
                let x2 = fi (1. -. x.bu) /. x.au in
                let x3 = fi (-.x.bu /. x.au) in

                if y >= x0 && y < x1 then x.ad *. f y +. x.bd
                else if y >= x1 && y <= x2 then 1.
                else if y > x2 && y <= x3 then x.au *. f y +. x.bu
                else 0.
                
