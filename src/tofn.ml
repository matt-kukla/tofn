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

let sametype x y = x.ofn_type == y.ofn_type

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

let inv f =
match f with 
    | Trapezoidal -> fun x -> x
    | Gaussian -> fun x -> -0.5 *. (x**2.)
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
    if (x.au = 0. && x.ad != 0.) || ((x.au != 0. && x.ad = 0.)) then true
    else if (x.au > 0. && x.ad > 0.) || (x.au < 0. && x.ad < 0.) then false
    else if let a = (inv x.ofn_type) (x.bd -. x.bu) /. (x.au -. x.ad)
        in (0. <= a ) && (a < 1.) then false
    else true

let membership x = 
    if not (is_proper x) then raise Improper_OFN else
    let mem f fi y =
        if is_increasing x then
            if y >= (x.au *. (f 0.) +. x.bu) && y < (x.au *. (f 1.) +. x.bu) 
                then fi ((y -. x.bu) /. x.au)
            else if y > (x.ad *. (f 1.) +. x.bd) &&  y <= (x.au *. (f 0.) +. x.bu)
                then fi ((y -. x.bd) /. x.ad)
            else 1. 

        else if (is_decreasing x) then
            if y >= (x.au *. (f 0.) +. x.bu) && y < (x.au *. (f 1.) +. x.bu)
                then fi ((y -. x.bd) /. x.ad)
            else if y > (x.ad *. (f 1.) +. x.bd) &&  y <= (x.au *. (f 0.) +. x.bu)
                then fi ((y -. x.bu) /. x.au)
            else 1.
        
        else raise Improper_OFN

    in 
        match x.ofn_type with
            | Trapezoidal -> mem (fun z ->  z) (inv Trapezoidal)  
            | Gaussian -> mem (fun z -> -2. *. log z) (inv Gaussian) 
            | Exponential -> mem (fun z -> exp z) (inv Exponential)

let conv_ofn x f = {ofn_type=f ; au = x.au; bu = x.bu; ad = x.ad; bd = x.bd;}
